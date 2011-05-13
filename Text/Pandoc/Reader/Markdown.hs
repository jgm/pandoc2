{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Reader.Markdown
where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Builder
import Data.Sequence as Seq
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.Foldable as F
import Text.Parsec hiding (sepBy)
import Control.Monad
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Data.Generics.Uniplate.Operations (transformBi)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity (lookupEntity)

-- Document-level parsers

pDoc :: PMonad m => P m Blocks
pDoc = skipMany pNewline *> pBlocks <* skipMany pNewline <* eof >>= pResolveRefs

pResolveRefs :: PMonad m => Blocks -> P m Blocks
pResolveRefs bs = do
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

-- Inline parsers

pInline :: PMonad m => P m Inlines
pInline = choice [ pTxt, pSp, pEndline, pFours, pStrong, pEmph, pVerbatim,
            pImage, pLink, pAutolink, pEscaped, pEntity, pHtmlInline, pSymbol ]

pInlines :: PMonad m => P m Inlines
pInlines = toInlines <$> many1 pInline

pInlinesBetween :: (Show b, PMonad m) => P m a -> P m b -> P m Inlines
pInlinesBetween start end = mconcat <$> try (start *> many1Till inner end)
  where inner =  (pSp >>= (\i -> (i <>) <$> (notFollowedBy' end *> pInline)))
             <|> pInline

pVerbatim :: PMonad m => P m Inlines
pVerbatim = try $ do
  delim <- many1 (char '`')
  sps
  let end = try $ sps *> string delim *> notFollowedBy (char '`')
  verbatim <$> textTill (nonnl <|> (' ' <$ pEndline)) end

pEscaped :: PMonad m => P m Inlines
pEscaped = txt . T.singleton <$> escapedChar

escapedChar :: Monad m => P m Char
escapedChar = try $ char '\\' *> oneOf "\\`*_{}[]()>#+-.!~"

-- like anyChar, but process markdown escapes
anyChar' :: Monad m => P m Char
anyChar' = escapedChar <|> anyChar

pSymbol :: PMonad m => P m Inlines
pSymbol = txt . T.singleton <$> nonnl

pSp :: PMonad m => P m Inlines
pSp = spaceChar *>
    (  skipMany1 spaceChar *> ((lineBreak <$ pEndline) <|> return (inline Sp))
   <|> return (inline Sp)
    )

pAutolink :: PMonad m => P m Inlines
pAutolink = (mkLink <$> pUri) <|> (mkEmail <$> pEmail)
  where mkLink u  = link (txt u) Source{ location = escapeURI u, title = "" }
        mkEmail u = link (txt u) Source{ location = escapeURI ("mailto:" <> u),
                                          title = "" }

pEmail :: PMonad m => P m Text
pEmail = try $ do
  char '<'
  xs <- text1Till nonSpaceChar (char '@')
  ys <- textTill nonSpaceChar (char '>')
  return $ xs <> "@" <> ys

pUri :: PMonad m => P m Text
pUri = try $ do
  char '<'
  xs <- text1Till nonSpaceChar (char ':')
  guard $ T.toLower xs `elem` ["http", "https", "ftp",
                               "file", "mailto", "news", "telnet" ]
  ys <- textTill nonnl (char '>')
  return $ xs <> ":" <> ys

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
  let lab = Label $ delink ils
  let ref = Ref{ key = Key ils, fallback = "[" <> ils <> "]" }
  pExplicitLink lab <|> pReferenceLink lab ref

pReferenceLink :: PMonad m => Label -> Source -> P m Inlines
pReferenceLink lab x = try $ do
  (k, fall) <- option (key x, fallback x) $ try $ do
                   s <- option mempty $ inline Sp <$
                           (() <$ pEndline <|> skipMany1 spaceChar)
                   ils <- pBracketedInlines
                   let k' = if ils == mempty then key x else Key ils
                   let f' = fallback x <> s <> "[" <> ils <> "]"
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
pSource = T.pack <$> (angleSource <|> regSource)
  where angleSource = try $ char '<'
                       *> manyTill (notFollowedBy nl *> anyChar') (char '>')
        regSource = concat <$> many chunk
        chunk       = many1 normalChar <|> inParens normalChar <|> count 1 (char '(')
        normalChar  = notFollowedBy paren *> lookAhead nonSpaceChar *> anyChar'

paren :: Monad m => P m Char
paren = satisfy $ \c -> c == '(' || c == ')'

-- | Parse between parentheses, including balanced parentheses.
inParens :: Monad m => P m Char -> P m String
inParens p = try $ do
  char '('
  xs <- many (inParens p <|> count 1 p)
  char ')'
  return $ '(': concat xs ++ ")"

pTitle :: PMonad m => P m Text
pTitle = do
  c <- quoteChar
  let end = try $ char c *> lookAhead (sps *> char ')')
  textTill anyChar' end

pTxt :: PMonad m => P m Inlines
pTxt = do
  x <- letter
  let txtchar = letter <|> (try $ char '_' <* lookAhead txtchar)
  xs <- many txtchar
  return $ txt $ T.pack (x:xs)

pFours :: PMonad m => P m Inlines
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- (char '*' <|> char '_')
  count 2 $ char x
  rest <- many1 (char x)
  return $ txt $ T.pack $ x : x : x : rest

pEmph :: PMonad m => P m Inlines
pEmph = emph <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = char '*' *> lookAhead nonSpaceChar
          starEnd   = notFollowedBy' pStrong *> char '*'
          ulStart   = char '_' *> lookAhead nonSpaceChar
          ulEnd     = notFollowedBy' pStrong *> char '_'

pStrong :: PMonad m => P m Inlines
pStrong = strong <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = string "**" *> lookAhead nonSpaceChar
          starEnd   = try (string "**")
          ulStart   = string "__" *> lookAhead nonSpaceChar
          ulEnd     = try (string "__")

-- Block parsers

pBlock :: PMonad m => P m Blocks
pBlock = choice [pQuote, pCode, pHrule, pList, pReference,
                 pHeader, pHtmlBlock, pPara]

pBlocks :: PMonad m => P m Blocks
pBlocks = mconcat <$> option [] (pBlock `sepBy` pNewlines)

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
  ils <- toInlines <$> many1Till pInline pNewline
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
    withEndline (notFollowedBy $ sps *> listStart) $ do
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
  spaceChar <|> lookAhead (nl <|> '\n' <$ eof)
  -- not an hrule
  notFollowedBy $ sps *> char b *> sps *> char b *> sps
                  *> skipMany (char b *> sps) *> nl
  return b

enum :: PMonad m => P m Char
enum = try $ nonindentSpace *> ('#' <$ many1 digit <|> char '#') <* char '.' <*
              (spaceChar <|> lookAhead (nl <|> '\n' <$ eof))

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

pReference :: PMonad m => P m Blocks
pReference = try $ do
  nonindentSpace
  k <- Key <$> pBracketedInlines
  char ':'
  spOptNl
  loc <- pSource
  tit <- option "" $ try $ spOptNl *> pRefTitle
  eol
  let src = Source{ location = escapeURI loc, title = tit }
  modifyState $ \st -> st{ sReferences = M.insert k src $ sReferences st }
  return mempty

pRefTitle :: PMonad m => P m Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = char start *>
          textTill nonnl (try $ char end *> eol)

-- HTML related parsers

pHtmlInline :: PMonad m => P m Inlines
pHtmlInline = rawInline (Format "html")
           <$> (pHtmlComment <|> snd <$> pHtmlTag)

pHtmlBlock :: PMonad m => P m Blocks
pHtmlBlock = rawBlock (Format "html")
          <$> (pHtmlComment <|> pHtmlBlockRaw)

pHtmlComment :: PMonad m => P m Text
pHtmlComment = try $ do
  string "<!--"
  x <- textTill anyChar (try $ string "-->")
  return $ "<!--" <> x <> "-->"

pInColumn1 :: Monad m => P m ()
pInColumn1 = do
  pos <- getPosition
  guard $ sourceColumn pos == 1

pHtmlBlockRaw :: PMonad m => P m Text
pHtmlBlockRaw = try $ do
  pInColumn1
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
               w  <- pInColumn1 *> pTagClose tagname
               eol
               return $ x <> ws <> w

blockTags :: [String]
blockTags = [ "address", "blockquote", "center", "del", "dir", "div",
              "dl", "fieldset", "form", "ins", "h1", "h2", "h3",
              "h4", "h5", "h6", "menu", "noframes", "noscript",
              "ol", "p", "pre", "table", "ul", "dd", "dt",
              "frameset", "li", "tbody", "td", "tfoot", "th",
              "thead", "tr", "script" ]

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

pTagClose :: PMonad m => String -> P m Text
pTagClose tagname = try $ do
  (t,n) <- pHtmlTag
  case t of
    [TagClose m] | map toLower m == tagname -> return n
    _ -> mzero
