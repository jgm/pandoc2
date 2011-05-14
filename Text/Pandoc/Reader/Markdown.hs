{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Reader.Markdown
where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Builder
import Text.Pandoc.Refs (resolveRefs)
import qualified Data.Sequence as Seq
import Data.Sequence (viewl, ViewL(..))
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower, isDigit)
import qualified Data.Foldable as F
import Text.Parsec hiding (sepBy, space, newline)
import Control.Monad
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity (lookupEntity)

-- Document-level parsers

pDoc :: PMonad m => P m Blocks
pDoc = skipMany newline *> pBlocks <* skipMany pNewline <* eof >>= resolveRefs

-- Inline parsers

pInline :: PMonad m => P m Inlines
pInline = choice [ pWord, pSp, pEndline, pFours, pStrong, pEmph, pVerbatim,
            pImage, pLink, pAutolink, pEscaped, pEntity, pHtmlInline, pSym ]

pInlines :: PMonad m => P m Inlines
pInlines = toInlines <$> many1 pInline

pInlinesBetween :: (Show b, PMonad m) => P m a -> P m b -> P m Inlines
pInlinesBetween start end = mconcat <$> try (start *> many1Till inner end)
  where inner      =  innerSpace <|> (notFollowedBy' pSp *> pInline)
        innerSpace = try $ pSp <* notFollowedBy' end

pVerbatim :: PMonad m => P m Inlines
pVerbatim = try $ do
  let backtick = sym '`'
  numticks <- length <$> many1 backtick
  sps
  let end = try $ sps *> count numticks backtick *> notFollowedBy backtick
  verbatim . toksToVerbatim
    <$> manyTill (nonNewline <|> (SPACE <$ pEndline)) end

pEscaped :: PMonad m => P m Inlines
pEscaped = try $ do
  sym '\\'
  strict <- getOption optStrict
  SYM c <- satisfyTok (if strict then isEscapable else isSymTok)
  return $ txt $ T.singleton c

isEscapable :: Tok -> Bool
isEscapable (SYM c) =
  c == '\\' || c == '`' || c == '*' || c == '_' ||
  c == '{'  || c == '}' || c == '[' || c == ']' ||
  c == '('  || c == ')' || c == '>' || c == '#' ||
  c == '+'  || c == '-' || c == '!' || c == '.'
isEscapable _ = False

pSym :: PMonad m => P m Inlines
pSym = do
  SYM c <- satisfyTok isSymTok
  return $ txt $ T.singleton c

pSp :: PMonad m => P m Inlines
pSp = space *> option (inline Sp)
        (skipMany1 space *> option (inline Sp) (lineBreak <$ pEndline))

pWord :: PMonad m => P m Inlines
pWord = do
  x <- satisfyTok isWordTok
  smart <- getOption optSmart
  let apos  = if smart
                 then try (SYM '\8217' <$ sym '\'' <*
                            lookAhead (satisfyTok isWordTok))
                 else mzero
  let chunk = satisfyTok isWordTok
           <|> (try $ sym '_' <* lookAhead chunk)
           <|> apos
  xs <- many chunk
  return $ txt $ toksToText (x:xs)

pAutolink :: PMonad m => P m Inlines
pAutolink = (mkLink <$> pUri) <|> (mkEmail <$> pEmail)
  where mkLink u  = link (txt u) Source{ location = escapeURI u, title = "" }
        mkEmail u = link (txt u) Source{ location = escapeURI ("mailto:" <> u),
                                          title = "" }

pEmail :: PMonad m => P m Text
pEmail = try $ do
  sym '<'
  xs <- text1Till nonSpace (sym '@')
  ys <- textTill nonSpace (sym '>')
  return $ xs <> "@" <> ys

pUri :: PMonad m => P m Text
pUri = try $ do
  sym '<'
  xs <- text1Till nonSpace (sym ':')
  guard $ T.toLower xs `elem` ["http", "https", "ftp",
                               "file", "mailto", "news", "telnet" ]
  ys <- textTill nonNewline (sym '>')
  return $ xs <> ":" <> ys

pBracketedInlines :: PMonad m => P m Inlines
pBracketedInlines = try $
  sym '[' *> (toInlines <$> manyTill pInline (sym ']'))

pImage :: PMonad m => P m Inlines
pImage = try $ do
  sym '!'
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
pReferenceLink lab ref = try $ do
  ref' <- option ref $ try $ do
              s <- option mempty $ (pEndline <|> pSp)
              ils <- pBracketedInlines
              let k' = if ils == mempty then key ref else Key ils
              let f' = fallback ref <> s <> "[" <> ils <> "]"
              return $ Ref{ key = k', fallback = f' }
  return $ inline $ Link lab ref'

pExplicitLink :: PMonad m => Label -> P m Inlines
pExplicitLink lab = try $ do
  sym '('
  sps
  src <- pSource
  tit <- option "" $ try $ spOptNl *> pTitle
  sps
  sym ')'
  return $ inline $ Link lab Source{ location = escapeURI src, title = tit }

pSource :: PMonad m => P m Text
pSource = angleSource <|> regSource
  where angleSource = try $ sym '<' *> textTill nonNewline (sym '>')
        regSource   = T.concat <$> many chunk
        chunk       = toksToText <$> (  many1 normalChar
                                    <|> inParens normalChar
                                    <|> count 1 (sym '(') )
        normalChar  = notFollowedBy paren *> lookAhead nonSpace *> anyTok
        paren       = sym '(' <|> sym ')'

-- | Parse between parentheses, including balanced parentheses.
inParens :: Monad m => P m Tok -> P m [Tok]
inParens p = try $ do
  sym '('
  xs <- many (inParens p <|> count 1 p)
  sym ')'
  return $ SYM '(': concat xs ++ [SYM ')']

pTitle :: PMonad m => P m Text
pTitle = do
  c <- quoteChar
  let end = try $ satisfyTok (== c) *> lookAhead (sps *> sym ')')
  textTill anyTok end

pFours :: PMonad m => P m Inlines
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- sym '*' <|> sym '_'
  count 2 $ satisfyTok (== x)
  rest <- many1 (satisfyTok (== x))
  return $ txt $ toksToText (x : x : x : rest)

pEmph :: PMonad m => P m Inlines
pEmph = emph <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = sym '*' *> lookAhead nonSpace
          starEnd   = notFollowedBy' pStrong *> sym '*'
          ulStart   = sym '_' *> lookAhead nonSpace
          ulEnd     = notFollowedBy' pStrong *> sym '_'

pStrong :: PMonad m => P m Inlines
pStrong = strong <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = count 2 (sym '*') *> lookAhead nonSpace
          starEnd   = try (count 2 (sym '*'))
          ulStart   = count 2 (sym '_') *> lookAhead nonSpace
          ulEnd     = try (count 2 (sym '_'))

-- Block parsers

pBlock :: PMonad m => P m Blocks
pBlock = choice [pQuote, pCode, pHrule, pList, pReference,
                 pHeader, pHtmlBlock, pPara]

pBlocks :: PMonad m => P m Blocks
pBlocks = option mempty $ mconcat <$> (pBlock `sepBy` pNewlines)

pPara :: PMonad m => P m Blocks
pPara = para <$> pInlines

pQuote :: PMonad m => P m Blocks
pQuote = try $ do
  let quoteStart = try $ nonindentSpace *> sym '>' *> optional space
  quoteStart
  xs <- withBlockSep quoteStart $ withEndline (optional quoteStart) pBlocks
  return $ quote xs

pHeader :: PMonad m => P m Blocks
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: PMonad m => P m Tok
setextChar = sym '=' <|> sym '-'

pHeaderSetext :: PMonad m => P m Blocks
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonNewline *> pNewline *> setextChar
  ils <- toInlines <$> many1Till pInline pNewline
  c <- setextChar
  skipMany (satisfyTok (== c))
  eol
  let level = if c == SYM '=' then 1 else 2
  return $ header level ils

pHeaderATX :: PMonad m => P m Blocks
pHeaderATX = try $ do
  level <- length <$> many1 (sym '#')
  sps
  let closeATX = try $ skipMany (sym '#') *> eol
  header level <$> toInlines <$> many1Till pInline closeATX

pList :: PMonad m => P m Blocks
pList = do
  (mark, style) <- lookAhead
                 $ ((enum, Ordered) <$ enum) <|> ((bullet, Bullet) <$ bullet)
  (tights, bs) <- unzip <$> many1 (pListItem mark)
  return $ block $ List ListAttr{ listTight = and tights, listStyle = style } bs

pListItem :: PMonad m
          => P m a -> P m (Bool, Blocks) -- True = suitable for tight list
pListItem start = try $ do
  n <- option 0 pNewlines
  start
  withBlockSep (indentSpace <|> eol) $
    withEndline (notFollowedBy $ sps *> listStart) $ do
      Blocks bs <- mconcat
                <$> (pBlock `sepBy` (pNewlines *> notFollowedBy spnl))
                   <|> return mempty
      if n > 1
         then return (False, Blocks bs)   -- not a tight list
         else case viewl bs of
                   EmptyL          -> return (True, Blocks bs)
                   (Para _ :< sq) ->
                        case viewl sq of
                              EmptyL               -> return (True, Blocks bs)
                              (List _ _ :< s)
                                 |  Seq.null s     -> return (True, Blocks bs)
                              _                    ->  return (False, Blocks bs)
                   _                -> return (False, Blocks bs)

listStart :: PMonad m => P m Tok
listStart = bullet <|> enum

bullet :: PMonad m => P m Tok
bullet = try $ do
  n <- nonindentSpace
  b <- sym '-' <|> sym '+' <|> sym '*'
  let bul = satisfyTok (== b)
  space <|> lookAhead newline
  tabstop <- getOption optTabStop
  -- if following spaces, gobble up to next tabstop
  case (tabstop - (n+2)) of
     x | x > 0 -> upto (tabstop - (n + 2)) space
     _         -> return []
  -- not an hrule
  notFollowedBy $ '\n' <$ (sps *> bul *> sps *> bul *> sps
                           *> skipMany (bul *> sps) *> newline)
  return b

enum :: PMonad m => P m Tok
enum = try $ do
  n <- nonindentSpace
  let digSym = satisfyTok isDigSym
      isDigSym (SYM d) = isDigit d
      isDigSym _       = False
  m <- length <$> (many digSym <|> count 1 (sym '#'))
  sym '.'
  space <|> lookAhead newline
  tabstop <- getOption optTabStop
  -- if following spaces, gobble up to next tabstop
  case (tabstop - (n+m+2)) of
     x | x > 0 -> upto (tabstop - (n + 2)) space
     _         -> return []
  return $ SYM '#'

pCode :: PMonad m => P m Blocks
pCode  = try $ do
  x <- indentSpace *> verbLine
  xs <- option []
        $ try $ pNewline *> sepBy ((indentSpace <|> eol) *> verbLine) pNewline
  return $ code $ T.unlines $ Prelude.reverse $ dropWhile T.null
         $ reverse (x:xs)

pHrule :: PMonad m => P m Blocks
pHrule = try $ do
  sps
  c <- sym '*' <|> sym '-' <|> sym '_'
  count 2 $ sps *> satisfyTok (== c)
  skipMany $ sps *> satisfyTok (== c)
  eol
  return hrule

pReference :: PMonad m => P m Blocks
pReference = try $ do
  nonindentSpace
  k <- Key <$> pBracketedInlines
  sym ':'
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
  where pRefTitleWith start end = sym start *>
          textTill nonNewline (try $ sym end *> eol)

-- HTML related parsers

pHtmlInline :: PMonad m => P m Inlines
pHtmlInline = rawInline (Format "html")
           <$> (pHtmlComment <|> snd <$> pHtmlTag)

pHtmlBlock :: PMonad m => P m Blocks
pHtmlBlock = rawBlock (Format "html")
          <$> (pHtmlComment <|> pHtmlBlockRaw)

pHtmlComment :: PMonad m => P m Text
pHtmlComment = try $ do
  sym '<' *> sym '!' *> sym '-' *> sym '-'
  x <- verbTextTill anyTok (try $ sym '-' *> sym '-' *> sym '>')
  return $ "<!--" <> x <> "-->"

pHtmlTag :: PMonad m => P m ([Tag String], Text)
pHtmlTag = try $ do
  sym '<'
  let anyNonNl = toksToVerbatim <$> count 1 nonNewline
  xs <- T.concat <$> manyTill (pQuoted <|> anyNonNl <|> "\n" <$ pNewline)
                      (sym '>')
  let t = "<" <> xs <> ">"
  case parseTags (T.unpack t) of
       (y:_) | isTagText y   -> mzero
       ys                    -> return (ys, t)

quoteChar :: Monad m => P m Tok
quoteChar = satisfyTok $ \c -> c == SYM '\'' || c == SYM '"'

-- | Parses a verbatim text between quote characters.
-- Returns the string and the quotes.
pQuoted :: PMonad m => P m Text
pQuoted = try $ do
  c@(SYM q) <- quoteChar
  x <- verbTextTill (nonNewline <|> SPACE <$ pEndline) (satisfyTok (== c))
  return $ T.singleton q <> x <> T.singleton q

pTagClose :: PMonad m => String -> P m Text
pTagClose tagname = try $ do
  (t,n) <- pHtmlTag
  case t of
    [TagClose m] | map toLower m == tagname -> return n
    _ -> mzero

pHtmlBlockRaw :: PMonad m => P m Text
pHtmlBlockRaw = try $ do
  pInColumn1
  (t:ts, x) <- pHtmlTag
  guard $ isTagOpen t
  tagname <- case t of
              (TagOpen s _)  | map toLower s `elem` blockTags -> return s
              _              -> mzero
  let notLt (SYM '<') = False
      notLt _         = True
  let chunk = notFollowedBy (pTagClose tagname) *> (pHtmlBlockRaw
              <|> toksToText <$> (many1 (satisfyTok notLt) <|> count 1 anyTok))
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

pEntity :: PMonad m => P m Inlines
pEntity = try $ do
  sym '&'
  x <- textTill nonSpace (sym ';')
  case lookupEntity (T.unpack x) of
       Just c   -> return $ txt $ T.singleton c
       _        -> mzero

