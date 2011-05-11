{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,  GeneralizedNewtypeDeriving,
    FlexibleInstances #-}
module Text.Pandoc.Reader.Markdown
where
import Text.Pandoc.Definition
import Text.Pandoc.Parsing
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
import qualified Data.Text.Encoding as E
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Data.Generics.Uniplate.Operations (transformBi)
import Network.URI ( escapeURIString, isAllowedInURI )
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity (lookupEntity)

pushEndline :: PMonad m => P m () -> P m ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

popEndline :: PMonad m => P m ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

withEndline :: PMonad m => P m a -> P m b -> P m b
withEndline sep p = pushEndline (sep *> return ()) *> p <* popEndline

pushBlockSep :: PMonad m => P m () -> P m ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

popBlockSep :: PMonad m => P m ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

withBlockSep :: PMonad m => P m a -> P m b -> P m b
withBlockSep sep p = pushBlockSep (sep *> return ()) *> p <* popBlockSep

pBlockSep :: PMonad m => P m ()
pBlockSep = try (getState >>= sequenceA . sBlockSep) >> return ()

pNewlines :: PMonad m => P m Int
pNewlines = Prelude.length <$> many1 pNewline

pNewline :: PMonad m => P m Int
pNewline = try $ spnl *> pBlockSep *> return 1

pEndline :: PMonad m => P m Inlines
pEndline = try $
  newline *> (getState >>= sequenceA . sEndline) *> skipMany spaceChar *>
  lookAhead nonnl *> return sp

pVerbatim :: PMonad m => P m Inlines
pVerbatim = try $ do
  delim <- many1 (char '`')
  sps
  verbatim . T.pack
     <$> many1Till (nonnl <|> (' ' <$ (pNewline *> notFollowedBy spnl)))
            (try $ sps *> string delim *> notFollowedBy (char '`'))

nonnl :: PMonad m => P m Char
nonnl = satisfy $ \c -> c /= '\n' && c /= '\r'

sps :: PMonad m => P m ()
sps = skipMany spaceChar

newline :: PMonad m => P m Char
newline = char '\n' <|> (char '\r' <* option '\n' (char '\n'))

spnl :: PMonad m => P m ()
spnl = try $ sps <* newline

eol :: PMonad m => P m ()
eol = sps *> lookAhead (() <$ newline <|> eof)

spOptNl :: PMonad m => P m ()
spOptNl = try $ sps <* optional (pNewline <* sps)

spaceChar :: PMonad m => P m Char
spaceChar = satisfy (\c -> c == ' ' || c == '\t')

nonSpaceChar :: PMonad m => P m Char
nonSpaceChar = satisfy  (\c -> c /= ' ' && c /= '\n' && c /= '\t')

showText :: Show a => a -> Text
showText = T.pack . show

logM :: PMonad m => LogLevel -> Text -> P m ()
logM level msg = do
  logLevel <- fmap sLogLevel getState
  pos <- getPosition
  msgs <- sMessages <$> getState
  if level >= logLevel
     then modifyState $ \st -> st{ sMessages = msgs |> Message level pos msg }
     else return ()

pInclude :: PMonad m => P m Blocks
pInclude = do
  f <- try (string "\\include{" *> manyTill anyChar (char '}'))
  inIncludes <- sInInclude <$> getState
  when (f `elem` inIncludes) $
    error $ "Recursive include in " <> show f
  modifyState $ \st -> st{ sInInclude = f : inIncludes }
  old <- getInput
  -- getFile <- sGetFile <$> getState
  lift (getFile f) >>= setInput
  skipMany pNewline
  bs <- pBlocks
  skipMany pNewline
  eof
  modifyState $ \st -> st{ sInInclude = inIncludes }
  setInput old
  return bs

parseWith :: PMonad m => P m a -> Text -> m a
parseWith p t = do
  let p' = do x <- p
              msgs <- sMessages <$> getState
              return (F.toList msgs, x)
  res <- runParserT p' pstate "input" t
  case res of
       Right (msgs, x) -> mapM_ addMessage msgs >> return x
       Left s          -> fail (show s)

pInline :: PMonad m => P m Inlines
pInline = choice [ pSp, pTxt, pEndline, pFours, pStrong, pEmph, pVerbatim,
                   pImage, pLink, pAutolink, pEscaped, pEntity, pHtmlInline, pSymbol ]

toInlines :: [Inlines] -> Inlines
toInlines = trimInlines . mconcat

pInlines :: PMonad m => P m Inlines
pInlines = toInlines <$> many1 pInline

pEscaped :: PMonad m => P m Inlines
pEscaped = txt . T.singleton <$> (try $ char '\\' *> oneOf "\\`*_{}[]()>#+-.!~")

pSymbol :: PMonad m => P m Inlines
pSymbol = txt . T.singleton <$> nonnl

pSp :: PMonad m => P m Inlines
pSp = spaceChar *> (  skipMany1 spaceChar *> ((lineBreak <$ pEndline) <|> return sp)
                  <|> return sp)

pAutolink :: PMonad m => P m Inlines
pAutolink = mkLink <$> pUri <|> mkEmail <$> pEmail
  where mkLink u = link (txt u) Source{ location = escapeURI u, title = "" }
        mkEmail u = link (txt u) Source{ location = escapeURI ("mailto:" <> u),
                                          title = "" }

pEmail :: PMonad m => P m Text
pEmail = try $ do
  char '<'
  xs <- many1Till nonSpaceChar (char '@')
  ys <- manyTill nonnl (char '>')
  return $ T.pack xs <> T.singleton '@' <> T.pack ys

pUri :: PMonad m => P m Text
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

pBracketedInlines :: PMonad m => P m Inlines
pBracketedInlines = try $
  char '[' *> (toInlines <$> manyTill pInline (char ']'))

pImage :: PMonad m => P m Inlines
pImage = try $ do
  char '!'
  [Link lab x] <- F.toList . unInlines <$> pLink
  return $ inline $ Image lab x

pLink :: PMonad m => P m Inlines
pLink = try $ do
  ils <- pBracketedInlines
  guard $ ils /= mempty
  let lab = Label ils
  let ref = Ref{ key = Key ils, fallback = txt "[" <> ils <> txt "]" }
  pExplicitLink lab <|> pReferenceLink lab ref

pReferenceLink :: PMonad m => Label -> Source -> P m Inlines
pReferenceLink lab x = try $ do
  (k, fall) <- option (key x, fallback x) $ try $ do
                   s <- option mempty $ sp <$
                           ((pNewline *> sps) <|> skipMany1 spaceChar)
                   ils <- pBracketedInlines
                   let k' = if ils == mempty then key x else Key ils
                   let f' = fallback x <> s <> txt "[" <> ils <> txt "]"
                   return (k',f')
  return $ inline $ Link lab Ref{ key = k, fallback = fall }

pExplicitLink :: PMonad m => Label -> P m Inlines
pExplicitLink lab = try $ do
  char '('
  sps
  src <- pSource
  tit <- option "" $ try $ spOptNl *> pTitle
  sps
  char ')'
  return $ inline $ Link lab Source{ location = escapeURI src, title = tit }

pSource :: PMonad m => P m Text
pSource = T.pack
       <$> ((char '<' *> manyTill nonnl (char '>'))
       <|> many (notFollowedBy (quoteChar <|> char ')') *> nonSpaceChar))

quoteChar :: PMonad m => P m Char
quoteChar = satisfy $ \c -> c == '\'' || c == '"'

pTitle :: PMonad m => P m Text
pTitle = do
  c <- quoteChar
  let end = try $ char c *> lookAhead (sps *> char ')')
  T.pack <$> manyTill anyChar end

pTxt :: PMonad m => P m Inlines
pTxt = do
  x <- letter
  let txtchar = letter <|> (try $ char '_' <* lookAhead txtchar)
  xs <- many txtchar
  return $ txt $ T.pack (x:xs)

pInlinesBetween :: PMonad m => P m a -> P m b -> P m Inlines
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

pFours :: PMonad m => P m Inlines
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- (char '*' <|> char '_')
  y <- char x
  z <- char x
  rest <- many1 (char x)
  return $ txt $ T.pack $ x : y : z : rest

pEmph :: PMonad m => P m Inlines
pEmph = emph <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = char '*' *> notFollowedBy (spaceChar <|> newline)
          starEnd   = notFollowedBy' pStrong *> char '*'
          ulStart   = char '_' *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = notFollowedBy' pStrong *> char '_'

pStrong :: PMonad m => P m Inlines
pStrong = strong <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = string "**" *> notFollowedBy (spaceChar <|> newline)
          starEnd   = try (string "**")
          ulStart   = string "__" *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = try (string "__")

trimInlines :: Inlines -> Inlines
trimInlines = Inlines . dropWhileL (== Sp) . dropWhileR (== Sp) . unInlines

pDoc :: PMonad m => P m Blocks
pDoc = skipMany pNewline *> pBlocks <* skipMany pNewline <* eof >>= resolveRefs

resolveRefs :: PMonad m => Blocks -> P m Blocks
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

pBlocks :: PMonad m => P m Blocks
pBlocks = mconcat <$> option [] (pBlock `sepBy` pNewlines)

pBlock :: PMonad m => P m Blocks
pBlock = choice [pQuote, pCode, pHrule, pList, pReference,
                 pHeader, pHtmlBlock, pInclude, pPara]

pBlank :: PMonad m => P m Blocks
pBlank = mempty <$ pNewlines

pPara :: PMonad m => P m Blocks
pPara = para <$> pInlines

pQuote :: PMonad m => P m Blocks
pQuote = quote <$> try (quoteStart
   *> withBlockSep quoteStart (withEndline (optional quoteStart) pBlocks))
    where quoteStart = try $ nonindentSpace *> char '>' *> optional spaceChar

pHeader :: PMonad m => P m Blocks
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: PMonad m => P m Char
setextChar = char '=' <|> char '-'

pHeaderSetext :: PMonad m => P m Blocks
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonnl *> pNewline *> setextChar
  ils <- toInlines <$> many1Till pInline newline
  c <- setextChar
  skipMany (char c)
  eol
  let level = if c == '=' then 1 else 2
  return $ header level ils

pHeaderATX :: PMonad m => P m Blocks
pHeaderATX = try $ do
  level <- Prelude.length <$> many1 (char '#')
  sps
  let closeATX = try $ skipMany (char '#') *> eol
  header level <$> toInlines <$> many1Till pInline closeATX

pList :: PMonad m => P m Blocks
pList = do
  (mark, style) <- lookAhead
                 $ ((enum, Ordered) <$ enum) <|> ((bullet, Bullet) <$ bullet)
  (tights, bs) <- unzip <$> many1 (pListItem mark)
  return $ block $ List ListAttr{ listTight = and tights, listStyle = style } bs

pListItem :: PMonad m => P m a -> P m (Bool, Blocks) -- True = suitable for tight list
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

listStart :: PMonad m => P m Char
listStart = bullet <|> enum

bullet :: PMonad m => P m Char
bullet = try $ do
  nonindentSpace
  b <- satisfy $ \c -> c == '-' || c == '+' || c == '*'
  spaceChar <|> lookAhead (newline <|> '\n' <$ eof)
  -- not an hrule
  notFollowedBy $ sps *> char b *> sps *> char b *> sps
                  *> skipMany (char b *> sps) *> newline
  return b

enum :: PMonad m => P m Char
enum = try $ nonindentSpace *> ('#' <$ many1 digit <|> char '#') <* char '.' <*
              (spaceChar <|> lookAhead (newline <|> '\n' <$ eof))

indentSpace :: PMonad m => P m ()
indentSpace = try $  (count 4 (char ' ') >> return ())
                 <|> (char '\t' >> return ())

nonindentSpace :: PMonad m => P m ()
nonindentSpace = option () $ onesp *> option () onesp *> option () onesp
  where onesp = () <$ char ' '

anyLine :: PMonad m => P m Text
anyLine = cleanup . T.pack <$> many nonnl
  where cleanup t = if T.all iswhite t then T.empty else t
        iswhite c = c == ' ' || c == '\t'

pCode :: PMonad m => P m Blocks
pCode  = try $ do
  x <- indentSpace *> anyLine
  xs <- option [] $ pNewline *> sepBy ((indentSpace <|> eol) *> anyLine) pNewline
  return $ code $ T.unlines $ Prelude.reverse $ dropWhile T.null
         $ Prelude.reverse (x:xs)

pHrule :: PMonad m => P m Blocks
pHrule = try $ do
  sps
  c <- satisfy $ \x -> x == '*' || x == '-' || x == '_'
  count 2 $ sps *> char c
  skipMany $ sps *> char c
  eol
  return hrule

-- redefined to include a 'try'
sepBy :: PMonad m => P m a -> P m b -> P m [a]
sepBy p sep = do
  x <- p
  xs <- many $ try (sep *> p)
  return (x:xs)

pReference :: PMonad m => P m Blocks
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

pRefTitle :: PMonad m => P m Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = T.pack <$> (char start *> manyTill nonnl
             (try $ char end *> lookAhead (() <$ spnl <|> eof)))

pQuoted :: PMonad m => P m String
pQuoted = try $ quoteChar >>= \c ->
  manyTill (nonnl <|> '\n' <$ pNewline) (char c) >>= \r ->
    return (c : r ++ [c])

pHtmlTag :: PMonad m => P m ([Tag String], Text)
pHtmlTag = try $ do
  char '<'
  xs <- concat
    <$> manyTill (pQuoted <|> count 1 nonnl <|> "\n" <$ pNewline) (char '>')
  let t = '<' : xs ++ ">"
  case parseTags t of
       (y:_) | isTagText y   -> mzero
       ys                    -> return (ys, T.pack t)

pEntity :: PMonad m => P m Inlines
pEntity = try $ do
  char '&'
  x <- manyTill nonSpaceChar (char ';')
  case lookupEntity x of
       Just c   -> return $ txt $ T.singleton c
       _        -> mzero

pHtmlInline :: PMonad m => P m Inlines
pHtmlInline = rawInline (Format "html") <$> (pHtmlComment <|> snd <$> pHtmlTag)

blockTags :: [String]
blockTags = [ "address", "blockquote", "center", "dir", "div",
              "dl", "fieldset", "form", "h1", "h2", "h3",
              "h4", "h5", "h6", "menu", "noframes", "noscript",
              "ol", "p", "pre", "table", "ul", "dd", "dt",
              "frameset", "li", "tbody", "td", "tfoot", "th",
              "thead", "tr", "script" ]

pHtmlBlock :: PMonad m => P m Blocks
pHtmlBlock = rawBlock (Format "html") <$> (pHtmlComment <|> pHtmlBlockRaw)

pHtmlComment :: PMonad m => P m Text
pHtmlComment = try $ do
  string "<!--"
  x <- manyTill anyChar (try $ string "-->")
  return $ "<!--" <> T.pack x <> "-->"

pHtmlBlockRaw :: PMonad m => P m Text
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

pTagClose :: PMonad m => String -> P m Text
pTagClose tagname = try $ do
  (t,n) <- pHtmlTag
  case t of
    [TagClose m] | map toLower m == tagname -> return n
    _ -> mzero
