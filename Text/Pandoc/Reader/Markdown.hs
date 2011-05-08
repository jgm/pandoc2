{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,  GeneralizedNewtypeDeriving #-}
module Text.Pandoc.Reader.Markdown
where
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.Sequence as Seq
import Data.Monoid
import qualified Data.Map as M
import Data.Traversable
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import Text.Parsec hiding (sepBy, newline)
import Control.Monad
import System.IO (stderr)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Data.Generics.Uniplate.Operations (transformBi)
import Network.URI ( escapeURIString, isAllowedInURI )
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity (lookupEntity)

data PState = PState {
                  sGetFile    :: FilePath -> P Text
                , sMessages   :: Seq Text
                , sLogLevel   :: LogLevel
                , sEndline    :: Seq (P ())
                , sBlockSep   :: Seq (P ())
                , sReferences :: M.Map Key Source
                }

pstate :: PState
pstate = PState { sGetFile  = undefined
                , sMessages = Seq.empty
                , sLogLevel = WARNING
                , sEndline  = Seq.empty
                , sBlockSep = Seq.empty
                , sReferences = M.empty
                }

type P a = ParsecT Text PState IO a

instance Stream Text IO Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

pushEndline :: P () -> P ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

popEndline :: P ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

withEndline :: P a -> P b -> P b
withEndline sep p = pushEndline (sep *> return ()) *> p <* popEndline

pushBlockSep :: P () -> P ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

popBlockSep :: P ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

withBlockSep :: P a -> P b -> P b
withBlockSep sep p = pushBlockSep (sep *> return ()) *> p <* popBlockSep

pBlockSep :: P ()
pBlockSep = try (getState >>= sequenceA . sBlockSep) >> return ()

pNewlines :: P Int
pNewlines = Prelude.length <$> many1 pNewline

pNewline :: P Int
pNewline = try $ spnl *> pBlockSep *> return 1

pEndline :: P Inlines
pEndline = try $
  newline *> (getState >>= sequenceA . sEndline) *> skipMany spaceChar *>
  lookAhead nonnl *> return sp

pVerbatim :: P Inlines
pVerbatim = try $ do
  delim <- many1 (char '`')
  sps
  verbatim . T.pack
     <$> many1Till (nonnl <|> (' ' <$ (pNewline *> notFollowedBy spnl)))
            (try $ sps *> string delim *> notFollowedBy (char '`'))

nonnl :: P Char
nonnl = satisfy $ \c -> c /= '\n' && c /= '\r'

sps :: P ()
sps = skipMany spaceChar

newline :: P Char
newline = char '\n' <|> (char '\r' <* option '\n' (char '\n'))

spnl :: P ()
spnl = try $ sps <* newline

eol :: P ()
eol = sps *> lookAhead (() <$ newline <|> eof)

spOptNl :: P ()
spOptNl = try $ sps <* optional (pNewline <* sps)

spaceChar :: P Char
spaceChar = satisfy (\c -> c == ' ' || c == '\t')

nonSpaceChar :: P Char
nonSpaceChar = satisfy  (\c -> c /= ' ' && c /= '\n' && c /= '\t')

showText :: Show a => a -> Text
showText = T.pack . show

logM :: LogLevel -> Text -> P ()
logM level msg = do
  logLevel <- fmap sLogLevel getState
  pos <- getPosition
  let msg' = showText level <> " (line " <>
             showText (sourceLine pos) <> " col " <>
             showText (sourceColumn pos) <> "): " <> msg
  if level >= logLevel
     then modifyState $ \st ->
              st { sMessages = sMessages st |> msg' }
     else return ()

parseWith :: P a -> Text -> IO a
parseWith p t = do
  let p' = do x <- p
              msgs <- fmap sMessages getState
              return (x, msgs)
  res <- runParserT p' pstate "input" t
  case res of
       Left err -> error $ show err
       Right (x, msgs) -> do
                   mapM_ (T.hPutStrLn stderr) $ F.toList msgs
                   return x

pInline :: P Inlines
pInline = choice [ pSp, pTxt, pEndline, pFours, pStrong, pEmph, pVerbatim,
                   pImage, pLink, pAutolink, pEscaped, pEntity, pHtmlInline, pSymbol ]

toInlines :: [Inlines] -> Inlines
toInlines = trimInlines . mconcat

pInlines :: P Inlines
pInlines = toInlines <$> many1 pInline

pEscaped :: P Inlines
pEscaped = txt . T.singleton <$> (try $ char '\\' *> oneOf "\\`*_{}[]()>#+-.!~")

pSymbol :: P Inlines
pSymbol = txt . T.singleton <$> nonnl

pSp :: P Inlines
pSp = spaceChar *> (  many1 spaceChar *> ((lineBreak <$ pEndline) <|> return sp)
                  <|> return sp)

pAutolink :: P Inlines
pAutolink = mkLink <$> pUri <|> mkEmail <$> pEmail
  where mkLink u = link (txt u) Source{ location = escapeURI u, title = "" }
        mkEmail u = link (txt u) Source{ location = escapeURI ("mailto:" <> u),
                                          title = "" }

pEmail :: P Text
pEmail = try $ do
  char '<'
  xs <- many1Till nonSpaceChar (char '@')
  ys <- manyTill nonnl (char '>')
  return $ T.pack xs <> T.singleton '@' <> T.pack ys

pUri :: P Text
pUri = try $ do
  char '<'
  xs <- many1Till nonSpaceChar (char ':')
  guard $ map toLower xs `elem` ["http", "https", "ftp",
                                 "file", "mailto", "news", "telnet" ]
  ys <- manyTill nonnl (char '>')
  return $ T.pack xs <> T.singleton ':' <> T.pack ys

many1Till :: Stream s m t
          => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p q = do
  x <- p
  xs <- manyTill p q
  return (x:xs)

pBracketedInlines :: P Inlines
pBracketedInlines = try $
  char '[' *> (toInlines <$> manyTill pInline (char ']'))

pImage :: P Inlines
pImage = try $ do
  char '!'
  [Link lab x] <- F.toList . unInlines <$> pLink
  return $ inline $ Image lab x

pLink :: P Inlines
pLink = try $ do
  ils <- pBracketedInlines
  guard $ ils /= mempty
  let lab = Label ils
  let ref = Ref{ key = Key ils, fallback = txt "[" <> ils <> txt "]" }
  pExplicitLink lab <|> pReferenceLink lab ref

pReferenceLink :: Label -> Source -> P Inlines
pReferenceLink lab x = try $ do
  (k, fall) <- option (key x, fallback x) $ try $ do
                   s <- option mempty $ sp <$
                           ((pNewline *> sps) <|> skipMany1 spaceChar)
                   ils <- pBracketedInlines
                   let k' = if ils == mempty then key x else Key ils
                   let f' = fallback x <> s <> txt "[" <> ils <> txt "]"
                   return (k',f')
  return $ inline $ Link lab Ref{ key = k, fallback = fall }

pExplicitLink :: Label -> P Inlines
pExplicitLink lab = try $ do
  char '('
  sps
  src <- pSource
  tit <- option "" $ try $ spOptNl *> pTitle
  sps
  char ')'
  return $ inline $ Link lab Source{ location = escapeURI src, title = tit }

pSource :: P Text
pSource = T.pack
       <$> ((char '<' *> manyTill nonnl (char '>'))
       <|> many (notFollowedBy (quoteChar <|> char ')') *> nonSpaceChar))

quoteChar :: P Char
quoteChar = satisfy $ \c -> c == '\'' || c == '"'

pTitle :: P Text
pTitle = do
  c <- quoteChar
  let end = try $ char c *> lookAhead (sps *> char ')')
  T.pack <$> manyTill anyChar end

pTxt :: P Inlines
pTxt = do
  x <- letter
  let txtchar = letter <|> (try $ char '_' <* lookAhead txtchar)
  xs <- many txtchar
  return $ txt $ T.pack (x:xs)

pInlinesBetween :: P a -> P b -> P Inlines
pInlinesBetween start end = mconcat <$> try (start *> many1Till pInline end)

-- | A more general form of @notFollowedBy@.  This one allows any~
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: (Stream s m t, Show b)
               => ParsecT s u m b -> ParsecT s u m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

pFours :: P Inlines
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- (char '*' <|> char '_')
  y <- char x
  z <- char x
  rest <- many1 (char x)
  return $ txt $ T.pack $ x : y : z : rest

pEmph :: P Inlines
pEmph = emph <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = char '*' *> notFollowedBy (spaceChar <|> newline)
          starEnd   = notFollowedBy' pStrong *> char '*'
          ulStart   = char '_' *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = notFollowedBy' pStrong *> char '_'

pStrong :: P Inlines
pStrong = strong <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = string "**" *> notFollowedBy (spaceChar <|> newline)
          starEnd   = try (string "**")
          ulStart   = string "__" *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = try (string "__")

trimInlines :: Inlines -> Inlines
trimInlines = Inlines . dropWhileL (== Sp) . dropWhileR (== Sp) . unInlines

pDoc :: P Blocks
pDoc = skipMany pNewline *> pBlocks <* skipMany pNewline <* eof >>= resolveRefs

resolveRefs :: Blocks -> P Blocks
resolveRefs bs = do
  refs <- sReferences <$> getState
  return $ transformBi (handleRef refs) bs

handleRef :: M.Map Key Source -> Inlines -> Inlines
handleRef refs (Inlines xs) = Inlines $ F.foldMap go xs
  where go (Link  lab Ref{ key = k, fallback = ils }) =
          case M.lookup k refs of
                 Just s  -> singleton $ Link lab s
                 Nothing -> unInlines ils
        go (Image lab Ref{ key = k, fallback = ils }) =
          case M.lookup k refs of
                 Just s  -> singleton $ Image lab s
                 Nothing -> unInlines ils
        go x = singleton x

pBlocks :: P Blocks
pBlocks = mconcat <$> option [] (pBlock `sepBy` pNewlines)

pBlock :: P Blocks
pBlock = choice [pQuote, pCode, pHrule, pList, pReference,
                 pHeader, pHtmlBlock, pPara]

pBlank :: P Blocks
pBlank = mempty <$ pNewlines

pPara :: P Blocks
pPara = para <$> pInlines

pQuote :: P Blocks
pQuote = quote <$> try (quoteStart
   *> withBlockSep quoteStart (withEndline (optional quoteStart) pBlocks))
    where quoteStart = try $ nonindentSpace *> char '>' *> optional spaceChar

pHeader :: P Blocks
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: P Char
setextChar = char '=' <|> char '-'

pHeaderSetext :: P Blocks
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonnl *> pNewline *> setextChar
  ils <- toInlines <$> many1Till pInline newline
  c <- setextChar
  skipMany (char c)
  eol
  let level = if c == '=' then 1 else 2
  return $ header level ils

pHeaderATX :: P Blocks
pHeaderATX = try $ do
  level <- Prelude.length <$> many1 (char '#')
  sps
  let closeATX = try $ skipMany (char '#') *> eol
  header level <$> toInlines <$> many1Till pInline closeATX

pList :: P Blocks
pList = do
  (mark, style) <- lookAhead
                 $ ((enum, Ordered) <$ enum) <|> ((bullet, Bullet) <$ bullet)
  (tights, bs) <- unzip <$> many1 (pListItem mark)
  return $ block $ List ListAttr{ listTight = and tights, listStyle = style } bs

pListItem :: P a -> P (Bool, Blocks) -- True = suitable for tight list
pListItem start = try $ do
  n <- option 0 pNewlines
  start
  withBlockSep (indentSpace <|> eol) $
    withEndline (notFollowedBy $ skipMany spaceChar *> listStart) $ do
      option ' ' $ char ' ' *> (option ' ' $ char ' ')
      Blocks bs <- mconcat
                <$> (pBlock `sepBy` (pNewlines >> notFollowedBy spnl))
                   <|> return mempty
      if n > 1
         then return (False, Blocks bs)
         else case viewl bs of
                   EmptyL          -> return (True, Blocks bs)
                   (Para _ :< sq) ->
                        case viewl sq of
                              EmptyL               -> return (True, Blocks bs)
                              (List _ _ :< s)
                                 |  Seq.null s     -> return (True, Blocks bs)
                              _                    ->  return (False, Blocks bs)
                   _                -> return (False, Blocks bs)

listStart :: P Char
listStart = bullet <|> enum

bullet :: P Char
bullet = try $ do
  nonindentSpace
  b <- satisfy $ \c -> c == '-' || c == '+' || c == '*'
  spaceChar <|> lookAhead (newline <|> '\n' <$ eof)
  -- not an hrule
  notFollowedBy $ sps *> char b *> sps *> char b *> sps
                  *> skipMany (char b *> sps) *> newline
  return b

enum :: P Char
enum = try $ nonindentSpace *> ('#' <$ many1 digit <|> char '#') <* char '.' <*
              (spaceChar <|> lookAhead (newline <|> '\n' <$ eof))

indentSpace :: P ()
indentSpace = try $  (count 4 (char ' ') >> return ())
                 <|> (char '\t' >> return ())

nonindentSpace :: P ()
nonindentSpace = option () $ onesp *> option () onesp *> option () onesp
  where onesp = () <$ char ' '

anyLine :: P Text
anyLine = cleanup . T.pack <$> many nonnl
  where cleanup t = if T.all iswhite t then T.empty else t
        iswhite c = c == ' ' || c == '\t'

pCode :: P Blocks
pCode  = try $ do
  x <- indentSpace *> anyLine
  xs <- option [] $ pNewline *> sepBy ((indentSpace <|> eol) *> anyLine) pNewline
  return $ code $ T.unlines $ Prelude.reverse $ dropWhile T.null
         $ Prelude.reverse (x:xs)

pHrule :: P Blocks
pHrule = try $ do
  sps
  c <- satisfy $ \x -> x == '*' || x == '-' || x == '_'
  count 2 $ sps *> char c
  skipMany $ sps *> char c
  eol
  return hrule

-- redefined to include a 'try'
sepBy :: P a -> P b -> P [a]
sepBy p sep = do
  x <- p
  xs <- many $ try (sep *> p)
  return (x:xs)

pReference :: P Blocks
pReference = try $ do
  nonindentSpace
  k <- Key <$> pBracketedInlines
  char ':'
  spOptNl
  loc <- T.pack <$> many1 (satisfy $ \c -> c /= ' ' && c /= '\n' && c /= '\t')
  tit <- option "" $ try $ spOptNl *> pRefTitle
  eol
  let src = Source{ location = escapeURI loc, title = tit }
  modifyState $ \st -> st{ sReferences = M.insert k src $ sReferences st }
  return mempty

escapeURI :: Text -> Text
escapeURI = T.pack . escapeURIString isAllowedInURI . B8.unpack . E.encodeUtf8

pRefTitle :: P Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = T.pack <$> (char start *> manyTill nonnl
             (try $ char end *> lookAhead (() <$ spnl <|> eof)))

pQuoted :: P String
pQuoted = try $ quoteChar >>= \c ->
  manyTill (nonnl <|> '\n' <$ pNewline) (char c) >>= \r ->
    return (c : r ++ [c])

pHtmlTag :: P ([Tag String], Text)
pHtmlTag = try $ do
  char '<'
  xs <- concat
    <$> manyTill (pQuoted <|> count 1 nonnl <|> "\n" <$ pNewline) (char '>')
  let t = '<' : xs ++ ">"
  case parseTags t of
       (y:_) | isTagText y   -> mzero
       ys                    -> return (ys, T.pack t)

pEntity :: P Inlines
pEntity = try $ do
  char '&'
  x <- manyTill nonSpaceChar (char ';')
  case lookupEntity x of
       Just c   -> return $ txt $ T.singleton c
       _        -> mzero

pHtmlInline :: P Inlines
pHtmlInline = rawInline (Format "html") <$> (pHtmlComment <|> snd <$> pHtmlTag)

blockTags :: [String]
blockTags = [ "address", "blockquote", "center", "dir", "div",
              "dl", "fieldset", "form", "h1", "h2", "h3",
              "h4", "h5", "h6", "menu", "noframes", "noscript",
              "ol", "p", "pre", "table", "ul", "dd", "dt",
              "frameset", "li", "tbody", "td", "tfoot", "th",
              "thead", "tr", "script" ]

pHtmlBlock :: P Blocks
pHtmlBlock = rawBlock (Format "html") <$> (pHtmlComment <|> pHtmlBlockRaw)

pHtmlComment :: P Text
pHtmlComment = try $ do
  string "<!--"
  x <- manyTill anyChar (try $ string "-->")
  return $ "<!--" <> T.pack x <> "-->"

pHtmlBlockRaw :: P Text
pHtmlBlockRaw = try $ do
  pos <- getPosition
  guard $ sourceColumn pos == 1
  (t:ts, x) <- pHtmlTag
  guard $ isTagOpen t
  tagname <- case t of
              (TagOpen s _)  | map toLower s `elem` blockTags -> return s
              _              -> mzero
  let chunk = notFollowedBy (pTagClose tagname) *> (pHtmlBlockRaw
                <|> T.pack <$> (many1 (satisfy (/='<')) <|> count 1 anyChar))
  case ts of
       [TagClose s] | map toLower s == tagname -> return x
       _ -> do ws <- mconcat <$> many chunk
               w <- pTagClose tagname
               return $ x <> ws <> w

pTagClose :: String -> P Text
pTagClose tagname = try $ do
  (t,n) <- pHtmlTag
  case t of
    [TagClose m] | map toLower m == tagname -> return n
    _ -> mzero