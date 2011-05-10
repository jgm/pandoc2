{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,  GeneralizedNewtypeDeriving,
    FlexibleInstances #-}
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
import Control.Monad.Trans
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Data.Generics.Uniplate.Operations (transformBi)
import Network.URI ( escapeURIString, isAllowedInURI )
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity (lookupEntity)

data Monad m => PState m =
         PState { sGetFile    :: FilePath -> P m Text
                , sInInclude  :: [FilePath]
                , sMessages   :: Seq Message
                , sLogLevel   :: LogLevel
                , sEndline    :: Seq (P m ())
                , sBlockSep   :: Seq (P m ())
                , sReferences :: M.Map Key Source
                }

pstate :: Monad m => PState m
pstate = PState { sGetFile    = \f -> logM WARNING ("Could not include file " <> T.pack f)
                                    >> return mempty
                , sInInclude  = []
                , sMessages   = Seq.empty
                , sLogLevel   = WARNING
                , sEndline    = Seq.empty
                , sBlockSep   = Seq.empty
                , sReferences = M.empty
                }

type P m a = ParsecT Text (PState m) m a

instance Monad m => Stream Text m Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

data Message = Message LogLevel SourcePos Text

instance Show Message where
  show (Message level pos t) = show level ++ " (line " ++
             show (sourceLine pos) ++ " col " ++
             show (sourceColumn pos) ++ "): " ++ T.unpack t

pushEndline :: Monad m => P m () -> P m ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

popEndline :: Monad m => P m ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

withEndline :: Monad m => P m a -> P m b -> P m b
withEndline sep p = pushEndline (sep *> return ()) *> p <* popEndline

pushBlockSep :: Monad m => P m () -> P m ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

popBlockSep :: Monad m => P m ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

withBlockSep :: Monad m => P m a -> P m b -> P m b
withBlockSep sep p = pushBlockSep (sep *> return ()) *> p <* popBlockSep

pBlockSep :: Monad m => P m ()
pBlockSep = try (getState >>= sequenceA . sBlockSep) >> return ()

pNewlines :: Monad m => P m Int
pNewlines = Prelude.length <$> many1 pNewline

pNewline :: Monad m => P m Int
pNewline = try $ spnl *> pBlockSep *> return 1

pEndline :: Monad m => P m Inlines
pEndline = try $
  newline *> (getState >>= sequenceA . sEndline) *> skipMany spaceChar *>
  lookAhead nonnl *> return sp

pVerbatim :: Monad m => P m Inlines
pVerbatim = try $ do
  delim <- many1 (char '`')
  sps
  verbatim . T.pack
     <$> many1Till (nonnl <|> (' ' <$ (pNewline *> notFollowedBy spnl)))
            (try $ sps *> string delim *> notFollowedBy (char '`'))

nonnl :: Monad m => P m Char
nonnl = satisfy $ \c -> c /= '\n' && c /= '\r'

sps :: Monad m => P m ()
sps = skipMany spaceChar

newline :: Monad m => P m Char
newline = char '\n' <|> (char '\r' <* option '\n' (char '\n'))

spnl :: Monad m => P m ()
spnl = try $ sps <* newline

eol :: Monad m => P m ()
eol = sps *> lookAhead (() <$ newline <|> eof)

spOptNl :: Monad m => P m ()
spOptNl = try $ sps <* optional (pNewline <* sps)

spaceChar :: Monad m => P m Char
spaceChar = satisfy (\c -> c == ' ' || c == '\t')

nonSpaceChar :: Monad m => P m Char
nonSpaceChar = satisfy  (\c -> c /= ' ' && c /= '\n' && c /= '\t')

showText :: Show a => a -> Text
showText = T.pack . show

logM :: Monad m => LogLevel -> Text -> P m ()
logM level msg = do
  logLevel <- fmap sLogLevel getState
  pos <- getPosition
  msgs <- sMessages <$> getState
  if level >= logLevel
     then modifyState $ \st -> st{ sMessages = msgs |> Message logLevel pos msg }
     else return ()

data Result a = Success { messages :: [Message], document :: a }
              | Failure ParseError
              deriving Show

pInclude :: Monad m => P m Blocks
pInclude = do
  f <- try (string "\\include{" *> manyTill anyChar (char '}'))
  inIncludes <- sInInclude <$> getState
  when (f `elem` inIncludes) $
    error $ "Recursive include in " <> show f
  modifyState $ \st -> st{ sInInclude = f : inIncludes }
  old <- getInput
  getFile <- sGetFile <$> getState
  getFile f >>= setInput
  skipMany pNewline
  bs <- pBlocks
  skipMany pNewline
  eof
  modifyState $ \st -> st{ sInInclude = inIncludes }
  setInput old
  return bs

parseWith :: P (Either ParseError) a -> Text -> Result a
parseWith p t =
  let p' = do x <- p
              msgs <- sMessages <$> getState
              return (x, F.toList msgs)
  in  case runParserT p' pstate "input" t of
            Right (Left err)        -> Failure err
            Right (Right (x, msgs)) -> Success { messages = msgs, document = x }
            Left e                  -> Failure e

parseWithM :: MonadIO m => P m a -> Text -> m a
parseWithM p t = do
  let s = pstate { sGetFile = liftIO . T.readFile }
  let p' = do x <- p
              getState >>= F.mapM_ (liftIO . hPutStrLn stderr . show) . sMessages
              return x
  res <- runParserT p' s "input" t
  case res of
       Left err -> fail $ show err
       Right x  -> return x

pInline :: Monad m => P m Inlines
pInline = choice [ pSp, pTxt, pEndline, pFours, pStrong, pEmph, pVerbatim,
                   pImage, pLink, pAutolink, pEscaped, pEntity, pHtmlInline, pSymbol ]

toInlines :: [Inlines] -> Inlines
toInlines = trimInlines . mconcat

pInlines :: Monad m => P m Inlines
pInlines = toInlines <$> many1 pInline

pEscaped :: Monad m => P m Inlines
pEscaped = txt . T.singleton <$> (try $ char '\\' *> oneOf "\\`*_{}[]()>#+-.!~")

pSymbol :: Monad m => P m Inlines
pSymbol = txt . T.singleton <$> nonnl

pSp :: Monad m => P m Inlines
pSp = spaceChar *> (  many1 spaceChar *> ((lineBreak <$ pEndline) <|> return sp)
                  <|> return sp)

pAutolink :: Monad m => P m Inlines
pAutolink = mkLink <$> pUri <|> mkEmail <$> pEmail
  where mkLink u = link (txt u) Source{ location = escapeURI u, title = "" }
        mkEmail u = link (txt u) Source{ location = escapeURI ("mailto:" <> u),
                                          title = "" }

pEmail :: Monad m => P m Text
pEmail = try $ do
  char '<'
  xs <- many1Till nonSpaceChar (char '@')
  ys <- manyTill nonnl (char '>')
  return $ T.pack xs <> T.singleton '@' <> T.pack ys

pUri :: Monad m => P m Text
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

pBracketedInlines :: Monad m => P m Inlines
pBracketedInlines = try $
  char '[' *> (toInlines <$> manyTill pInline (char ']'))

pImage :: Monad m => P m Inlines
pImage = try $ do
  char '!'
  [Link lab x] <- F.toList . unInlines <$> pLink
  return $ inline $ Image lab x

pLink :: Monad m => P m Inlines
pLink = try $ do
  ils <- pBracketedInlines
  guard $ ils /= mempty
  let lab = Label ils
  let ref = Ref{ key = Key ils, fallback = txt "[" <> ils <> txt "]" }
  pExplicitLink lab <|> pReferenceLink lab ref

pReferenceLink :: Monad m => Label -> Source -> P m Inlines
pReferenceLink lab x = try $ do
  (k, fall) <- option (key x, fallback x) $ try $ do
                   s <- option mempty $ sp <$
                           ((pNewline *> sps) <|> skipMany1 spaceChar)
                   ils <- pBracketedInlines
                   let k' = if ils == mempty then key x else Key ils
                   let f' = fallback x <> s <> txt "[" <> ils <> txt "]"
                   return (k',f')
  return $ inline $ Link lab Ref{ key = k, fallback = fall }

pExplicitLink :: Monad m => Label -> P m Inlines
pExplicitLink lab = try $ do
  char '('
  sps
  src <- pSource
  tit <- option "" $ try $ spOptNl *> pTitle
  sps
  char ')'
  return $ inline $ Link lab Source{ location = escapeURI src, title = tit }

pSource :: Monad m => P m Text
pSource = T.pack
       <$> ((char '<' *> manyTill nonnl (char '>'))
       <|> many (notFollowedBy (quoteChar <|> char ')') *> nonSpaceChar))

quoteChar :: Monad m => P m Char
quoteChar = satisfy $ \c -> c == '\'' || c == '"'

pTitle :: Monad m => P m Text
pTitle = do
  c <- quoteChar
  let end = try $ char c *> lookAhead (sps *> char ')')
  T.pack <$> manyTill anyChar end

pTxt :: Monad m => P m Inlines
pTxt = do
  x <- letter
  let txtchar = letter <|> (try $ char '_' <* lookAhead txtchar)
  xs <- many txtchar
  return $ txt $ T.pack (x:xs)

pInlinesBetween :: Monad m => P m a -> P m b -> P m Inlines
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

pFours :: Monad m => P m Inlines
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- (char '*' <|> char '_')
  y <- char x
  z <- char x
  rest <- many1 (char x)
  return $ txt $ T.pack $ x : y : z : rest

pEmph :: Monad m => P m Inlines
pEmph = emph <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = char '*' *> notFollowedBy (spaceChar <|> newline)
          starEnd   = notFollowedBy' pStrong *> char '*'
          ulStart   = char '_' *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = notFollowedBy' pStrong *> char '_'

pStrong :: Monad m => P m Inlines
pStrong = strong <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = string "**" *> notFollowedBy (spaceChar <|> newline)
          starEnd   = try (string "**")
          ulStart   = string "__" *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = try (string "__")

trimInlines :: Inlines -> Inlines
trimInlines = Inlines . dropWhileL (== Sp) . dropWhileR (== Sp) . unInlines

pDoc :: Monad m => P m Blocks
pDoc = skipMany pNewline *> pBlocks <* skipMany pNewline <* eof >>= resolveRefs

resolveRefs :: Monad m => Blocks -> P m Blocks
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

pBlocks :: Monad m => P m Blocks
pBlocks = mconcat <$> option [] (pBlock `sepBy` pNewlines)

pBlock :: Monad m => P m Blocks
pBlock = choice [pQuote, pCode, pHrule, pList, pReference,
                 pHeader, pHtmlBlock, pInclude, pPara]

pBlank :: Monad m => P m Blocks
pBlank = mempty <$ pNewlines

pPara :: Monad m => P m Blocks
pPara = para <$> pInlines

pQuote :: Monad m => P m Blocks
pQuote = quote <$> try (quoteStart
   *> withBlockSep quoteStart (withEndline (optional quoteStart) pBlocks))
    where quoteStart = try $ nonindentSpace *> char '>' *> optional spaceChar

pHeader :: Monad m => P m Blocks
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: Monad m => P m Char
setextChar = char '=' <|> char '-'

pHeaderSetext :: Monad m => P m Blocks
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonnl *> pNewline *> setextChar
  ils <- toInlines <$> many1Till pInline newline
  c <- setextChar
  skipMany (char c)
  eol
  let level = if c == '=' then 1 else 2
  return $ header level ils

pHeaderATX :: Monad m => P m Blocks
pHeaderATX = try $ do
  level <- Prelude.length <$> many1 (char '#')
  sps
  let closeATX = try $ skipMany (char '#') *> eol
  header level <$> toInlines <$> many1Till pInline closeATX

pList :: Monad m => P m Blocks
pList = do
  (mark, style) <- lookAhead
                 $ ((enum, Ordered) <$ enum) <|> ((bullet, Bullet) <$ bullet)
  (tights, bs) <- unzip <$> many1 (pListItem mark)
  return $ block $ List ListAttr{ listTight = and tights, listStyle = style } bs

pListItem :: Monad m => P m a -> P m (Bool, Blocks) -- True = suitable for tight list
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

listStart :: Monad m => P m Char
listStart = bullet <|> enum

bullet :: Monad m => P m Char
bullet = try $ do
  nonindentSpace
  b <- satisfy $ \c -> c == '-' || c == '+' || c == '*'
  spaceChar <|> lookAhead (newline <|> '\n' <$ eof)
  -- not an hrule
  notFollowedBy $ sps *> char b *> sps *> char b *> sps
                  *> skipMany (char b *> sps) *> newline
  return b

enum :: Monad m => P m Char
enum = try $ nonindentSpace *> ('#' <$ many1 digit <|> char '#') <* char '.' <*
              (spaceChar <|> lookAhead (newline <|> '\n' <$ eof))

indentSpace :: Monad m => P m ()
indentSpace = try $  (count 4 (char ' ') >> return ())
                 <|> (char '\t' >> return ())

nonindentSpace :: Monad m => P m ()
nonindentSpace = option () $ onesp *> option () onesp *> option () onesp
  where onesp = () <$ char ' '

anyLine :: Monad m => P m Text
anyLine = cleanup . T.pack <$> many nonnl
  where cleanup t = if T.all iswhite t then T.empty else t
        iswhite c = c == ' ' || c == '\t'

pCode :: Monad m => P m Blocks
pCode  = try $ do
  x <- indentSpace *> anyLine
  xs <- option [] $ pNewline *> sepBy ((indentSpace <|> eol) *> anyLine) pNewline
  return $ code $ T.unlines $ Prelude.reverse $ dropWhile T.null
         $ Prelude.reverse (x:xs)

pHrule :: Monad m => P m Blocks
pHrule = try $ do
  sps
  c <- satisfy $ \x -> x == '*' || x == '-' || x == '_'
  count 2 $ sps *> char c
  skipMany $ sps *> char c
  eol
  return hrule

-- redefined to include a 'try'
sepBy :: Monad m => P m a -> P m b -> P m [a]
sepBy p sep = do
  x <- p
  xs <- many $ try (sep *> p)
  return (x:xs)

pReference :: Monad m => P m Blocks
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

pRefTitle :: Monad m => P m Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = T.pack <$> (char start *> manyTill nonnl
             (try $ char end *> lookAhead (() <$ spnl <|> eof)))

pQuoted :: Monad m => P m String
pQuoted = try $ quoteChar >>= \c ->
  manyTill (nonnl <|> '\n' <$ pNewline) (char c) >>= \r ->
    return (c : r ++ [c])

pHtmlTag :: Monad m => P m ([Tag String], Text)
pHtmlTag = try $ do
  char '<'
  xs <- concat
    <$> manyTill (pQuoted <|> count 1 nonnl <|> "\n" <$ pNewline) (char '>')
  let t = '<' : xs ++ ">"
  case parseTags t of
       (y:_) | isTagText y   -> mzero
       ys                    -> return (ys, T.pack t)

pEntity :: Monad m => P m Inlines
pEntity = try $ do
  char '&'
  x <- manyTill nonSpaceChar (char ';')
  case lookupEntity x of
       Just c   -> return $ txt $ T.singleton c
       _        -> mzero

pHtmlInline :: Monad m => P m Inlines
pHtmlInline = rawInline (Format "html") <$> (pHtmlComment <|> snd <$> pHtmlTag)

blockTags :: [String]
blockTags = [ "address", "blockquote", "center", "dir", "div",
              "dl", "fieldset", "form", "h1", "h2", "h3",
              "h4", "h5", "h6", "menu", "noframes", "noscript",
              "ol", "p", "pre", "table", "ul", "dd", "dt",
              "frameset", "li", "tbody", "td", "tfoot", "th",
              "thead", "tr", "script" ]

pHtmlBlock :: Monad m => P m Blocks
pHtmlBlock = rawBlock (Format "html") <$> (pHtmlComment <|> pHtmlBlockRaw)

pHtmlComment :: Monad m => P m Text
pHtmlComment = try $ do
  string "<!--"
  x <- manyTill anyChar (try $ string "-->")
  return $ "<!--" <> T.pack x <> "-->"

pHtmlBlockRaw :: Monad m => P m Text
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

pTagClose :: Monad m => String -> P m Text
pTagClose tagname = try $ do
  (t,n) <- pHtmlTag
  case t of
    [TagClose m] | map toLower m == tagname -> return n
    _ -> mzero
