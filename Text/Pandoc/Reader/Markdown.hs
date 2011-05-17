{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Reader.Markdown
where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Builder
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower, isDigit)
import Text.Parsec hiding (sepBy, space, newline)
import Control.Monad
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Text.HTML.TagSoup

-- Document-level parsers

pDoc :: PMonad m => P m Blocks
pDoc = skipMany newline *> pBlocks <* skipMany pNewline <* eof
    >>= finalResult

-- Inline parsers

pInline :: PMonad m => P m (PR Inlines)
pInline = choice [ pWord, pSp, pEndline, pQuoted, pFours,
            pStrong, pEmph, pVerbatim, pNoteRef, pImage, pLink, pAutolink,
            pInlineNote, pEscaped, pMath, pEntity, pHtmlInline, pSym ]

pInlines :: PMonad m => P m (PR Inlines)
pInlines = trimInlines <$$> mconcat <$> many1 pInline

pInlinesBetween :: (Show b, PMonad m) => P m a -> P m b -> P m (PR Inlines)
pInlinesBetween start end = mconcat <$> try (start *> many1Till inner end)
  where inner      =  innerSpace <|> (notFollowedBy' pSp *> pInline)
        innerSpace = try $ pSp <* notFollowedBy' end

pVerbatim :: PMonad m => P m (PR Inlines)
pVerbatim = try $ do
  let backtick = sym '`'
  numticks <- length <$> many1 backtick
  sps
  let end = try $ sps *> count numticks backtick *> notFollowedBy backtick
  Const . verbatim . toksToVerbatim <$>
     manyTill (nonNewline <|> (SPACE <$ pEndline)) end

pEscaped :: PMonad m => P m (PR Inlines)
pEscaped = try $ do
  sym '\\'
  strict <- getOption optStrict
  SYM c <- satisfyTok (if strict then isEscapable else isSymTok)
  return $ Const $ ch c

isEscapable :: Tok -> Bool
isEscapable (SYM c) =
  c == '\\' || c == '`' || c == '*' || c == '_' ||
  c == '{'  || c == '}' || c == '[' || c == ']' ||
  c == '('  || c == ')' || c == '>' || c == '#' ||
  c == '+'  || c == '-' || c == '!' || c == '.'
isEscapable _ = False

pSym :: PMonad m => P m (PR Inlines)
pSym = do
  smart <- getOption optSmart
  SYM c <- satisfyTok isSymTok
  Const <$>
    case c of
       '.' | smart -> option (ch '.') pEllipses
       '-' | smart -> option (ch '-') (pEnDash <|> pEmDash)
       _           -> return (ch c)

pEllipses :: PMonad m => P m Inlines
pEllipses = try $ -- we've already parsed one '.'
  ch '\8230' <$ (sym '.' *> sym '.')

pEnDash :: PMonad m => P m Inlines
pEnDash = -- we've already parsed one '-'
  ch '\8211' <$ lookAhead pDigit
   where pDigit = satisfyTok $ \t ->
                     case t of
                          SYM d | isDigit d -> True
                          _                 -> False

pEmDash :: PMonad m => P m Inlines
pEmDash = -- we've already parsed one '-'
  ch '\8212' <$ (sym '-' *> optional (sym '-'))

pSp :: PMonad m => P m (PR Inlines)
pSp = space *> option (Const $ single Sp)
        (skipMany1 space *>
          option (Const $ single Sp) ((Const lineBreak) <$ pEndline))

pWord :: PMonad m => P m (PR Inlines)
pWord = do
  x <- wordTok
  smart <- getOption optSmart
  let apos  = if smart
                 then try (SYM '\8217' <$ sym '\'' <* lookAhead wordTok)
                 else mzero
  let chunk = satisfyTok isWordTok
           <|> (try $ sym '_' <* lookAhead chunk)
           <|> apos
  xs <- many chunk
  return $ Const $ txt $ toksToText (x:xs)

pAutolink :: PMonad m => P m (PR Inlines)
pAutolink = (Const . mkLink <$> pUri) <|> (Const . mkEmail <$> pEmail)
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

pBracketedInlines :: PMonad m => P m (PR Inlines)
pBracketedInlines = try $
  sym '[' *> (trimInlines <$$> mconcat <$> manyTill pInline (sym ']'))

pImage :: PMonad m => P m (PR Inlines)
pImage = try $ do
  sym '!'
  res <- pLink
  return $ Future $ \s ->
    mapItems linkToImage $ evalResult s res
      where linkToImage (Link l s) = single (Image l s)
            linkToImage (Txt t)    = single (Txt $ "!" <> t)
            linkToImage x          = single x

pLink :: PMonad m => P m (PR Inlines)
pLink = try $ do
  ils <- pBracketedInlines
  pExplicitLink ils <|> pReferenceLink ils

pReferenceLink :: PMonad m => PR Inlines -> P m (PR Inlines)
pReferenceLink lab = try $ do
  (ref, fallback) <-
    option (lab, "[" <> lab <> "]") $ try $ do
      s <- option mempty $ (pEndline <|> pSp)
      ils <- pBracketedInlines
      let f = "[" <> lab <> "]" <> s <> "[" <> ils <> "]"
      let ils' = case ils of
                      Const x | x == mempty -> lab
                      _                     -> ils
      return (ils', f)
  return $ Future $ \s ->
    case M.lookup (Key $ evalResult nullReferences ref) (rLinks s) of
         Nothing  -> evalResult s fallback
         Just src -> link (delink $ evalResult s lab) src

pExplicitLink :: PMonad m => PR Inlines -> P m (PR Inlines)
pExplicitLink lab = try $ do
  sym '('
  sps
  src <- pSource
  tit <- option "" $ try $ spOptNl *> pTitle
  sps
  sym ')'
  return $ Future $ \s ->
             link (delink $ evalResult s lab)
               Source{ location = escapeURI src, title = tit }

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

pQuoted :: PMonad m => P m (PR Inlines)
pQuoted = try $ do
  getOption optSmart >>= guard
  SYM c <- satisfyTok isSymTok <|> SYM <$> pEntityChar
  case c of
       '\'' -> option (Const $ ch '\8217') $
                 pQuotedWith SingleQuoted pInline
       '"'  -> option (Const $ txt "\"") $
                 pQuotedWith DoubleQuoted pInline
       _    -> mzero

pFours :: PMonad m => P m (PR Inlines)
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- sym '*' <|> sym '_'
  count 2 $ satisfyTok (== x)
  rest <- many1 (satisfyTok (== x))
  return $ Const $ txt $ toksToText (x : x : x : rest)

pEmph :: PMonad m => P m (PR Inlines)
pEmph = emph <$$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = sym '*' *> lookAhead nonSpace
          starEnd   = notFollowedBy' pStrong *> sym '*'
          ulStart   = sym '_' *> lookAhead nonSpace
          ulEnd     = notFollowedBy' pStrong *> sym '_'

pStrong :: PMonad m => P m (PR Inlines)
pStrong = strong <$$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = count 2 (sym '*') *> lookAhead nonSpace
          starEnd   = try (count 2 (sym '*'))
          ulStart   = count 2 (sym '_') *> lookAhead nonSpace
          ulEnd     = try (count 2 (sym '_'))

pNoteRef :: PMonad m => P m (PR Inlines)
pNoteRef = do
  k <- pNoteMarker
  return $ Future $ \refs ->
    case M.lookup k (rNotes refs) of
          Just (Const bs) -> note bs
          Just (Future f) -> note $ f refs
          Nothing         -> txt k

pNoteMarker :: PMonad m => P m Text
pNoteMarker = try $ do
  sym '[' *> sym '^'
  x <- text1Till nonSpace (sym ']')
  -- the key is also the fallback, so we wrap in [^...]
  return $ "[^" <> x <> "]"

pInlineNote :: PMonad m => P m (PR Inlines)
pInlineNote = note . para
  <$$> (unlessStrict *> try (sym '^' *> pBracketedInlines))

-- Block parsers

pBlock :: PMonad m => P m (PR Blocks)
pBlock = choice [pQuote, pCode, pHrule, pList, pNote, pReference,
                 pHeader, pHtmlBlock, pPara]

pBlocks :: PMonad m => P m (PR Blocks)
pBlocks = option mempty $ mconcat <$> (pBlock `sepBy` pNewlines)

pPara :: PMonad m => P m (PR Blocks)
pPara = para <$$> pInlines

pQuote :: PMonad m => P m (PR Blocks)
pQuote = try $ do
  let start = try $ nonindentSpace *> sym '>' *> optional space
  start
  quote <$$> withBlockSep start (withEndline (optional start) pBlocks)

pHeader :: PMonad m => P m (PR Blocks)
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: PMonad m => P m Tok
setextChar = sym '=' <|> sym '-'

pHeaderSetext :: PMonad m => P m (PR Blocks)
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonNewline *> pNewline *> setextChar
  ils <- trimInlines <$$> mconcat <$> many1Till pInline pNewline
  c <- setextChar
  skipMany (satisfyTok (== c))
  eol
  let level = if c == SYM '=' then 1 else 2
  header level <$$> return ils

pHeaderATX :: PMonad m => P m (PR Blocks)
pHeaderATX = try $ do
  level <- length <$> many1 (sym '#')
  sps
  let closeATX = try $ skipMany (sym '#') *> eol
  header level . trimInlines <$$> mconcat <$> many1Till pInline closeATX

pList :: PMonad m => P m (PR Blocks)
pList = do
  (mark, style) <- lookAhead
                 $ ((enum, Ordered) <$ enum) <|> ((bullet, Bullet) <$ bullet)
  (tights, bs) <- unzip <$> many1 (pListItem mark)
  return $ Future $ \s ->
    single $ List ListAttr{ listTight = and tights, listStyle = style }
           $ map (evalResult s) bs

pListItem :: PMonad m
          => P m a -> P m (Bool, PR Blocks) -- True = suitable for tight list
pListItem start = try $ do
  n <- option 0 pNewlines
  start
  withBlockSep (indentSpace <|> eol) $
    withEndline (notFollowedBy $ sps *> listStart) $ do
      bs <- mconcat
            <$> (pBlock `sepBy` (pNewlines *> notFollowedBy spnl))
                <|> return mempty
      if n > 1
         then return (False, bs)   -- not a tight list
         else case toItems (evalResult nullReferences bs) of
                   []                  -> return (True, bs)
                   [Para _]            -> return (True, bs)
                   [Para _, List _ _ ] -> return (True, bs)
                   _                   -> return (False, bs)

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

pCode :: PMonad m => P m (PR Blocks)
pCode  = try $ do
  x <- indentSpace *> verbLine
  xs <- option []
        $ try $ pNewline *> sepBy ((indentSpace <|> eol) *> verbLine) pNewline
  return $ Const $ code
         $ T.unlines $ Prelude.reverse $ dropWhile T.null $ reverse (x:xs)

pHrule :: PMonad m => P m (PR Blocks)
pHrule = try $ do
  sps
  c <- sym '*' <|> sym '-' <|> sym '_'
  count 2 $ sps *> satisfyTok (== c)
  skipMany $ sps *> satisfyTok (== c)
  eol
  return $ Const hrule

pReference :: PMonad m => P m (PR Blocks)
pReference = try $ do
  nonindentSpace
  k <- pBracketedInlines
  sym ':'
  spOptNl
  loc <- pSource
  tit <- option "" $ try $ spOptNl *> pRefTitle
  eol
  let src = Source{ location = escapeURI loc, title = tit }
  refs <- getReferences
  let key = Key $ evalResult nullReferences k
  setReferences refs{ rLinks = M.insert key src $ rLinks refs }
  return mempty

pRefTitle :: PMonad m => P m Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = sym start *>
          textTill nonNewline (try $ sym end *> eol)

pNote :: PMonad m => P m (PR Blocks)
pNote = try $ do
  nonindentSpace
  k <- pNoteMarker
  sym ':'
  bs <- withBlockSep (indentSpace <|> eol) $ do
          mconcat <$> (pBlock `sepBy` (pNewlines *> notFollowedBy spnl))
  refs <- getReferences
  setReferences refs{ rNotes = M.insert k bs $ rNotes refs }
  return mempty

-- HTML related parsers

pHtmlInline :: PMonad m => P m (PR Inlines)
pHtmlInline = Const . rawInline (Format "html")
           <$> (pHtmlComment <|> snd <$> pHtmlTag)

pHtmlBlock :: PMonad m => P m (PR Blocks)
pHtmlBlock = Const . rawBlock (Format "html")
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
  xs <- T.concat <$> manyTill (pQuotedText <|> anyNonNl <|> "\n" <$ pNewline)
                      (sym '>')
  let t = "<" <> xs <> ">"
  case parseTags (T.unpack t) of
       (y:_) | isTagText y   -> mzero
       ys                    -> return (ys, t)

quoteChar :: Monad m => P m Tok
quoteChar = satisfyTok $ \c -> c == SYM '\'' || c == SYM '"'

-- | Parses a verbatim text between quote characters.
-- Returns the string and the quotes.
pQuotedText :: PMonad m => P m Text
pQuotedText = try $ do
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

pEntity :: PMonad m => P m (PR Inlines)
pEntity = Const . ch <$> pEntityChar

unlessStrict :: PMonad m => P m ()
unlessStrict = getOption optStrict >>= guard . not

---

pMathWord :: PMonad m => P m Text
pMathWord = mconcat <$> (many1 mathChunk)
  where mathChunk = (sym '\\' *> (("\\" <>) . tokToVerbatim <$> anyTok))
                 <|> (tokToVerbatim <$> normalMath)
        normalMath = satisfyTok $ \t ->
                        t /= SPACE && t /= NEWLINE && t /= SYM '$'
        tokToVerbatim t = toksToVerbatim [t]

pMath :: PMonad m => P m (PR Inlines)
pMath = unlessStrict *>
  try (do
    sym '$'
    display <- option False (True <$ sym '$')
    raw <- if display
              then notFollowedBy (sym '$') *> pMathDisplay
              else notFollowedBy (space <|> newline) *> pMathInline
    raw' <- pApplyMacros' raw
    let mt = if display then DisplayMath else InlineMath
    return $ Const $ math mt raw')

pApplyMacros' :: PMonad m => Text -> P m Text
pApplyMacros' = return . id -- TODO

pMathDisplay :: PMonad m => P m Text
pMathDisplay = try (verbTextTill normal mark)
  where mark   = try $ sym '$' *> sym '$'
        normal = (nonNewline <|> SYM '\n' <$ pEndline)

pMathInline :: PMonad m => P m Text
pMathInline = try $ do
  words' <- sepBy1 pMathWord (skipMany1 $ pSp <|> pEndline)
  sym '$'
  let digitTok (SYM d) = isDigit d
      digitTok _       = False
  notFollowedBy $ satisfyTok digitTok
  return $ T.unwords words'
