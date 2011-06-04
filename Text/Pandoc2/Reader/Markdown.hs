{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc2.Reader.Markdown (markdownDoc, markdownInlines)
where
import Text.Pandoc2.Definition
import Text.Pandoc2.Shared
import Text.Pandoc2.Parsing.Generic
import Text.Pandoc2.Parsing.Types
import Text.Pandoc2.Parsing.PMonad
import Text.Pandoc2.Parsing.TextTok
import Text.Pandoc2.Builder
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower, isDigit, ord)
import Text.Parsec hiding (sepBy, space, newline)
import Control.Monad
import Control.Applicative ((<$>), (<$), (*>), (<*), liftA2)
import Data.Traversable (sequenceA)
import Text.HTML.TagSoup

type MP m a = P Tok m a

markdownDoc :: PMonad m => POptions -> Text -> m Blocks
markdownDoc opts t = parseWith opts pDoc $ tokenize opts t

markdownInlines :: PMonad m => POptions -> Text -> m Inlines
markdownInlines opts t = parseWith opts pInlines $ tokenize opts t

-- Document-level parsers

pDoc :: PMonad m => MP m (PR Blocks)
pDoc = skipMany newline *> pBlocks <* skipMany pNewline <* eof

-- Inline parsers

pInline :: PMonad m => MP m (PR Inlines)
pInline = choice [ pWord, pSp, pEndline, pQuoted, pFours,
            pStrong, pEmph, pSuperscript, pSubscript, pStrikeout,
            pVerbatim, pNoteRef, pImage, pLink, pAutolink,
            pInlineNote, pEscaped, pMath, pEntity, pHtmlInline, pSym ]

pInlines :: PMonad m => MP m (PR Inlines)
pInlines = trimInlines <$$> mconcat <$> many1 pInline

pInlinesBetween :: (Show b, PMonad m) => MP m a -> MP m b -> MP m (PR Inlines)
pInlinesBetween start end = mconcat <$> try (start *> many1Till inner end)
  where inner      =  innerSpace <|> (notFollowedBy' pSp *> pInline)
        innerSpace = try $ pSp <* notFollowedBy' end

pVerbatim :: PMonad m => MP m (PR Inlines)
pVerbatim = try $ do
  let backtick = sym '`'
  numticks <- length <$> many1 backtick
  sps
  let end = try $ sps *> count numticks backtick *> notFollowedBy backtick
  Const . verbatim . toksToVerbatim <$>
     manyTill (nonNewline <|> (SPACE <$ pEndline)) end

pEscaped :: PMonad m => MP m (PR Inlines)
pEscaped = try $ do
  sym '\\'
  exts <- getOption optExtensions
  SYM c <- satisfyTok $ if isEnabled All_symbols_escapable exts
                           then isSymTok
                           else isEscapable
  return $ Const $ ch c

isEscapable :: Tok -> Bool
isEscapable (SYM c) =
  c == '\\' || c == '`' || c == '*' || c == '_' ||
  c == '{'  || c == '}' || c == '[' || c == ']' ||
  c == '('  || c == ')' || c == '>' || c == '#' ||
  c == '+'  || c == '-' || c == '!' || c == '.'
isEscapable _ = False

pSym :: PMonad m => MP m (PR Inlines)
pSym = do
  smart <- getOption optSmart
  SYM c <- satisfyTok isSymTok
  Const <$>
    case c of
       '.' | smart -> option (ch '.') pEllipses
       '-' | smart -> option (ch '-') (pEnDash <|> pEmDash)
       _           -> return (ch c)

pEllipses :: PMonad m => MP m Inlines
pEllipses = try $ -- we've already parsed one '.'
  ch '\8230' <$ (sym '.' *> sym '.')

pEnDash :: PMonad m => MP m Inlines
pEnDash = -- we've already parsed one '-'
  ch '\8211' <$ lookAhead pDigit
   where pDigit = satisfyTok $ \t ->
                     case t of
                          SYM d | isDigit d -> True
                          _                 -> False

pEmDash :: PMonad m => MP m Inlines
pEmDash = -- we've already parsed one '-'
  ch '\8212' <$ (sym '-' *> optional (sym '-'))

pSp :: PMonad m => MP m (PR Inlines)
pSp = space *> option (Const $ single Sp)
        (skipMany1 space *>
          option (Const $ single Sp) ((Const lineBreak) <$ pEndline))

pWord :: PMonad m => MP m (PR Inlines)
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

pAutolink :: PMonad m => MP m (PR Inlines)
pAutolink = (Const . mkLink <$> pUri) <|> (Const . mkEmail <$> pEmail)
  where mkLink u  = link (txt u) Source{ location = escapeURI u, title = "" }
        mkEmail u = link (txt u) Source{ location = escapeURI ("mailto:" <> u),
                                          title = "" }

pEmail :: PMonad m => MP m Text
pEmail = try $ do
  sym '<'
  xs <- text1Till nonSpace (sym '@')
  ys <- textTill nonSpace (sym '>')
  return $ xs <> "@" <> ys

pUri :: PMonad m => MP m Text
pUri = try $ do
  sym '<'
  WORD x <- wordTok
  sym ':'
  guard $ T.toLower x `elem` ["http", "https", "ftp",
                              "file", "mailto", "news", "telnet" ]
  y <- textTill nonNewline (sym '>')
  return $ x <> ":" <> y

pBracketedInlines :: PMonad m => MP m (PR Inlines)
pBracketedInlines = try $
  sym '[' *> (trimInlines <$$> mconcat <$> manyTill pInline (sym ']'))

pImage :: PMonad m => MP m (PR Inlines)
pImage = try $ do
  sym '!'
  res <- pLink
  return $ Future $ \s ->
    mapItems linkToImage $ evalResult s res
      where linkToImage (Link l s) = single (Image l s)
            linkToImage (Txt t)    = single (Txt $ "!" <> t)
            linkToImage x          = single x

pLink :: PMonad m => MP m (PR Inlines)
pLink = try $ do
  ils <- pBracketedInlines
  pExplicitLink ils <|> pReferenceLink ils

pReferenceLink :: PMonad m => PR Inlines -> MP m (PR Inlines)
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

pExplicitLink :: PMonad m => PR Inlines -> MP m (PR Inlines)
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

pSource :: PMonad m => MP m Text
pSource = angleSource <|> regSource
  where angleSource = try $ sym '<' *> textTill nonNewline (sym '>')
        regSource   = T.concat <$> many chunk
        chunk       = toksToText <$> (  many1 normalChar
                                    <|> inParens normalChar
                                    <|> count 1 (sym '(') )
        normalChar  = notFollowedBy paren *> lookAhead nonSpace *> anyTok
        paren       = sym '(' <|> sym ')'

-- | Parse between parentheses, including balanced parentheses.
inParens :: PMonad m => MP m Tok -> MP m [Tok]
inParens p = try $ do
  sym '('
  xs <- many (inParens p <|> count 1 p)
  sym ')'
  return $ SYM '(': concat xs ++ [SYM ')']

pTitle :: PMonad m => MP m Text
pTitle = do
  c <- quoteChar
  let end = try $ satisfyTok (== c) *> lookAhead (sps *> sym ')')
  textTill anyTok end

pQuoted :: PMonad m => MP m (PR Inlines)
pQuoted = try $ do
  guard =<< getOption optSmart
  SYM c <- satisfyTok isSymTok <|> SYM <$> pEntityChar
  case c of
       '\'' -> option (Const $ ch '\8217') $
                 pQuotedWith SingleQuoted pInline
       '"'  -> option (Const $ txt "\"") $
                 pQuotedWith DoubleQuoted pInline
       _    -> mzero

pFours :: PMonad m => MP m (PR Inlines)
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- sym '*' <|> sym '_'
  count 2 $ satisfyTok (== x)
  rest <- many1 (satisfyTok (== x))
  return $ Const $ txt $ toksToText (x : x : x : rest)

pEmph :: PMonad m => MP m (PR Inlines)
pEmph = emph <$$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = sym '*' *> lookAhead nonSpace
          starEnd   = notFollowedBy' pStrong *> sym '*'
          ulStart   = sym '_' *> lookAhead nonSpace
          ulEnd     = notFollowedBy' pStrong *> sym '_'

pStrong :: PMonad m => MP m (PR Inlines)
pStrong = strong <$$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = count 2 (sym '*') *> lookAhead nonSpace
          starEnd   = try (count 2 (sym '*'))
          ulStart   = count 2 (sym '_') *> lookAhead nonSpace
          ulEnd     = try (count 2 (sym '_'))

pSuperscript :: PMonad m => MP m (PR Inlines)
pSuperscript = superscript <$$> (pInlinesBetween superDelim superDelim)
  where superDelim = sym '^' <* notFollowedBy (sym '[') -- footnote

pSubscript :: PMonad m => MP m (PR Inlines)
pSubscript = subscript <$$> (pInlinesBetween subDelim subDelim)
  where subDelim = try $ sym '~' <* notFollowedBy (sym '~')

pStrikeout :: PMonad m => MP m (PR Inlines)
pStrikeout = strikeout <$$> (pInlinesBetween strikeStart strikeEnd)
  where strikeStart = try $ sym '~' <* sym '~'
        strikeEnd   = try $ sym '~' *> sym '~'

pNoteRef :: PMonad m => MP m (PR Inlines)
pNoteRef = do
  k <- pNoteMarker
  return $ Future $ \refs ->
    case M.lookup k (rNotes refs) of
          Just (Const bs) -> note bs
          Just (Future f) -> note $ f refs
          Nothing         -> txt k

pNoteMarker :: PMonad m => MP m Text
pNoteMarker = try $ do
  sym '[' *> sym '^'
  x <- text1Till nonSpace (sym ']')
  -- the key is also the fallback, so we wrap in [^...]
  return $ "[^" <> x <> "]"

pInlineNote :: PMonad m => MP m (PR Inlines)
pInlineNote = note . para
  <$$> (guardExtension Footnotes *> try (sym '^' *> pBracketedInlines))

-- Block parsers

pBlock :: PMonad m => MP m (PR Blocks)
pBlock = choice [pQuote, pCode, pHrule, pList, pNote, pReference,
                 pHeader, pHtmlBlock, pDefinitions, pPara]

pBlocks :: PMonad m => MP m (PR Blocks)
pBlocks = option mempty $ mconcat <$> (pBlock `sepBy` pNewlines)

pPara :: PMonad m => MP m (PR Blocks)
pPara = para <$$> pInlines

pQuote :: PMonad m => MP m (PR Blocks)
pQuote = try $ do
  let start = try $ nonindentSpace *> sym '>' *> optional space
  start
  quote <$$> withBlockSep start (withEndline (optional start) pBlocks)

pHeader :: PMonad m => MP m (PR Blocks)
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: PMonad m => MP m Tok
setextChar = sym '=' <|> sym '-'

pHeaderSetext :: PMonad m => MP m (PR Blocks)
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonNewline *> pNewline *> setextChar
  ils <- trimInlines <$$> withEndline mzero pInlines
  pNewline
  c <- setextChar
  skipMany (satisfyTok (== c))
  eol
  let level = if c == SYM '=' then 1 else 2
  header level <$$> return ils

pHeaderATX :: PMonad m => MP m (PR Blocks)
pHeaderATX = try $ do
  level <- length <$> many1 (sym '#')
  sps
  let closeATX = try $ skipMany (sym '#') *> eol
  header level . trimInlines <$$> mconcat <$> many1Till pInline closeATX

pDefinitions :: PMonad m => MP m (PR Blocks)
pDefinitions = try $ do
  guardExtension Definition_lists
  lookAhead $ manyTill anyTok pNewlines *> pDefSep
  (tights, items) <- unzip <$> many1 pDefinition
  let items' = sequenceA items
  let items'' = if and tights
                   then map (\(t,d) -> (t, map paraToPlain d)) <$> items'
                   else items'
  return $ definitions <$> items''

paraToPlain :: Blocks -> Blocks
paraToPlain = mapItems go
  where go :: Block -> Blocks
        go (Para xs)  = plain xs
        go x          = single x

pDefSep :: PMonad m => MP m ()
pDefSep = try $ nonindentSpace *> (sym '~' <|> sym ':') *> sps

pDef :: PMonad m => MP m (Bool, PR Blocks)
pDef = try $ do
  startNls <- option 0 pNewlines
  pDefSep
  bs <- withBlockSep (indentSpace <|> eol)
      $ withEndline indentSpace
      $ pBlock `sepBy` pNewlines
  return (startNls <= 1 && length bs == 1, mconcat bs)

pDefinition :: PMonad m => MP m (Bool, PR (Inlines, [Blocks]))
pDefinition = try $ do
  startNls <- option 0 pNewlines
  term <- withEndline mzero pInlines
  (tights, defs) <- unzip <$> many1 pDef
  return (startNls <= 1 && and tights, liftA2 (,) term (sequenceA defs))

pList :: PMonad m => MP m (PR Blocks)
pList = do
  marker <- lookAhead listStart
  (tights, bs) <- unzip <$> many1 (pListItem marker)
  let style = case marker of
                   BulletMarker _     -> Bullet
                   NumberMarker n s d -> Ordered (fromMaybe 1 n) s d
                   _                  -> error $ show marker <> " not supported"
  let bs' = sequenceA bs
  let bs'' = if and tights
                then map paraToPlain <$> bs'
                else bs'
  return $ single . List style <$> bs''

pListItem :: PMonad m
          => ListMarker
          -> MP m (Bool, PR Blocks) -- True = suitable for tight list
pListItem marker = try $ do
  n <- option 0 pNewlines
  m <- listStart
  guard $ m `continues` marker
  withBlockSep (indentSpace <|> eol) $
    withEndline (notFollowedBy $ sps *> listStart) $ do
      bs <- mconcat
            <$> (pBlock `sepBy` pNewlines) <|> return mempty
      if n > 1
         then return (False, bs)   -- not a tight list
         else case toItems (evalResult nullReferences bs) of
                   []                  -> return (True, bs)
                   [Para _]            -> return (True, bs)
                   [Para _, List _ _ ] -> return (True, bs)
                   _                   -> return (False, bs)

data ListMarker = BulletMarker Char
                | NumberMarker (Maybe Int) ListNumberStyle ListNumberDelim
                | ExampleMarker Text ListNumberStyle ListNumberDelim
                deriving Show

continues :: ListMarker -> ListMarker -> Bool
continues (BulletMarker c) (BulletMarker d) = c == d
continues (NumberMarker (Just 1) s1 d1) (NumberMarker (Just 8) s2 d2)
  | s1 == UpperRoman && s2 == UpperAlpha ||
    s1 == LowerRoman && s2 == LowerAlpha = d1 == d2 -- I continues H
continues (NumberMarker (Just n1) s1 d1) (NumberMarker (Just n2) s2 d2)
  | (s2 == UpperRoman && s1 == UpperAlpha ||
     s2 == LowerRoman && s1 == LowerAlpha) &&
    (  (n1 == 22 && n2 == 4)  -- v continues 4
    || (n1 == 24 && n2 == 9)  -- x continues 9
    || (n1 == 12 && n2 == 49) -- l continues 49
    || (n1 == 3 && n2 == 99)  -- c continues 99
    || (n1 == 4 && n2 == 499) -- d continues 499
    || (n1 == 12 && n2 == 999) -- m continues 999
    ) = d1 == d2
continues (NumberMarker _ s1 d1) (NumberMarker _ s2 d2) = s1 == s2 && d1 == d2
continues (ExampleMarker _ s1 d1) (ExampleMarker _ s2 d2) = s1 == s2 && d1 == d2
continues _ _ = False

listStart :: PMonad m => MP m ListMarker
listStart = bullet <|> enum

bullet :: PMonad m => MP m ListMarker
bullet = try $ do
  n <- nonindentSpace
  SYM x <- sym '-' <|> sym '+' <|> sym '*'
  space <|> lookAhead newline
  tabstop <- getOption optTabStop
  -- if following spaces, gobble up to next tabstop
  case (tabstop - (n+2)) of
     y | y > 0 -> upto (tabstop - (n + 2)) space
     _         -> return []
  -- not an hrule
  notFollowedBy $ '\n' <$ (sps *> sym x *> sps *> sym x *> sps
                           *> skipMany (sym x *> sps) *> newline)
  return $ BulletMarker x

enum :: PMonad m => MP m ListMarker
enum = try $ do
  n <- nonindentSpace
  lparen <- option False (True <$ (guardExtension Fancy_lists *> sym '('))
  (num, len, sty) <- pListNumber
  delim <- if lparen
              then TwoParens <$ sym ')'
              else (Period <$ sym '.')
                   <|> (OneParen <$ (guardExtension Fancy_lists *> sym ')'))
  let space' = if sty == UpperAlpha && delim == Period
                  then space *> space  -- to avoid interpreting initials
                  else space
  space' <|> lookAhead newline
  tabstop <- getOption optTabStop
  -- if following spaces, gobble up to next tabstop
  case (tabstop - (n + len + 2)) of
     x | x > 0 -> upto (tabstop - (n + 2)) space
     _         -> return []
  return $ NumberMarker num sty delim

pListNumber :: PMonad m => MP m (Maybe Int, Int, ListNumberStyle)
                                 -- (number, length, style)
pListNumber =  pDecimalNumber
           <|> (guardExtension Fancy_lists *> (pAlphaNumber <|> pRomanNumber))
  where pDecimalNumber = do
          ds <- many1 digSym <|> count 1 (sym '#')
          exts <- getOption optExtensions
          case ds of
               [SYM '#']  -> return (Nothing, 1, DefaultStyle)
               _          -> return (Just $ digitsToNum ds, length ds,
                                      if isEnabled Fancy_lists exts
                                         then Decimal
                                         else DefaultStyle)
        pAlphaNumber = try $ do
          WORD w <- wordTok
          case T.unpack w of
               -- we let I/i start Roman numeral list
               [c] | c >= 'a' && c <= 'z' && c /= 'i' ->
                     return (Just (1 + ord c - ord 'a'), 1, LowerAlpha)
               [c] | c >= 'A' && c <= 'Z' && c /= 'I' ->
                     return (Just (1 + ord c - ord 'A'), 1, UpperAlpha)
               _  -> mzero
        pRomanNumber = try $ do
          WORD w <- wordTok
          case fromRoman w of
               Just (n, sty) -> return (Just n, T.length w, sty)
               Nothing       -> mzero
        digSym           = satisfyTok isDigSym
        isDigSym (SYM d) = isDigit d
        isDigSym _       = False
        fromSym (SYM c)  | isDigit c = c
        fromSym _        = error "Non-digit in fromSym/pListNumber"
        digitsToNum      = read . map fromSym

pCode :: PMonad m => MP m (PR Blocks)
pCode = pCodeIndented <|> (guardExtension Delimited_code_blocks *> pCodeDelimited)

pCodeIndented :: PMonad m => MP m (PR Blocks)
pCodeIndented  = try $ do
  x <- indentSpace *> verbLine
  xs <- option []
        $ try $ pNewline *> sepBy ((indentSpace <|> eol) *> verbLine) pNewline
  return $ Const $ code
         $ T.unlines $ Prelude.reverse $ dropWhile T.null $ reverse (x:xs)

codeBlockDelimiter :: PMonad m => Int -> MP m (Int, Attr)
codeBlockDelimiter len = try $ do
  size <- length <$> many1 (sym '~')
  guard $ size >= len
  many space
  attr <- option nullAttr pAttributes
  pNewline
  return (size, attr)

pAttributes :: PMonad m => MP m Attr
pAttributes = try $ sym '{' *> sps *> (mconcat <$> (pAttribute `sepBy` sps))
                    <* sps <* sym '}'

pAttribute :: PMonad m => MP m Attr
pAttribute = pIdentifierAttr <|> pClassAttr <|> pKeyValAttr

pIdentifier :: PMonad m => MP m Text
pIdentifier = do
  first <- wordTok
  rest  <- many $ wordTok <|> sym '-' <|> sym '_' <|> sym ':' <|> sym '.'
  return $ toksToVerbatim (first:rest)

pIdentifierAttr :: PMonad m => MP m Attr
pIdentifierAttr = try $ do
  sym '#'
  x <- pIdentifier
  return $ Attr [("id", x)]

pClassAttr :: PMonad m => MP m Attr
pClassAttr = try $ do
  sym '.'
  x <- pIdentifier
  return $ Attr [("class", x)]

pKeyValAttr :: PMonad m => MP m Attr
pKeyValAttr = try $ do
  key <- pIdentifier
  sym '='
  val <- (T.drop 1 . T.init) <$> pQuotedText
  return $ Attr [(key, val)]

pCodeDelimited :: PMonad m => MP m (PR Blocks)
pCodeDelimited = try $ do
  (size, attr) <- codeBlockDelimiter 3
  contents <- manyTill (verbLine <* pNewline) (codeBlockDelimiter size)
  return $ Const $ codeAttr attr $ T.unlines contents

pHrule :: PMonad m => MP m (PR Blocks)
pHrule = try $ do
  sps
  c <- sym '*' <|> sym '-' <|> sym '_'
  count 2 $ sps *> satisfyTok (== c)
  skipMany $ sps *> satisfyTok (== c)
  eol
  return $ Const hrule

pReference :: PMonad m => MP m (PR Blocks)
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

pRefTitle :: PMonad m => MP m Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = sym start *>
          textTill nonNewline (try $ sym end *> eol)

pNote :: PMonad m => MP m (PR Blocks)
pNote = try $ do
  nonindentSpace
  k <- pNoteMarker
  sym ':'
  bs <- withBlockSep (indentSpace <|> eol) $ do
          mconcat <$> (pBlock `sepBy` pNewlines)
  refs <- getReferences
  setReferences refs{ rNotes = M.insert k bs $ rNotes refs }
  return mempty

-- HTML related parsers

pHtmlInline :: PMonad m => MP m (PR Inlines)
pHtmlInline = Const . rawInline (Format "html")
           <$> (pHtmlComment <|> snd <$> pHtmlTag)

pHtmlBlock :: PMonad m => MP m (PR Blocks)
pHtmlBlock = Const . rawBlock (Format "html")
          <$> (pHtmlComment <|> pHtmlBlockRaw)

pHtmlComment :: PMonad m => MP m Text
pHtmlComment = try $ do
  sym '<' *> sym '!'
  let twodash = try $ sym '-' *> sym '-'
  let subComment = (\x -> "--" <> toksToVerbatim x <> "--")
                <$> (twodash *> manyTill anyTok twodash)
  let subSpace   = toksToVerbatim <$> many1 space
  x <- mconcat <$>
        (manyTill (subComment <|> subSpace <|> "\n" <$ pNewline) (sym '>'))
  return $ "<!" <> x <> ">"

pHtmlTag :: PMonad m => MP m ([Tag String], Text)
pHtmlTag = try $ do
  sym '<'
  let anyNonNl = toksToVerbatim <$> count 1 nonNewline
  xs <- T.concat <$> manyTill (pQuotedText <|> anyNonNl <|> "\n" <$ pNewline)
                      (sym '>')
  let t = "<" <> xs <> ">"
  case parseTags (T.unpack t) of
       (y:_) | isTagText y   -> mzero
       ys                    -> return (ys, t)

quoteChar :: PMonad m => MP m Tok
quoteChar = satisfyTok $ \c -> c == SYM '\'' || c == SYM '"'

-- | Parses a verbatim text between quote characters.
-- Returns the string and the quotes.
pQuotedText :: PMonad m => MP m Text
pQuotedText = try $ do
  c@(SYM q) <- quoteChar
  x <- verbTextTill (nonNewline <|> SPACE <$ pEndline) (satisfyTok (== c))
  return $ T.singleton q <> x <> T.singleton q

pTagClose :: PMonad m => String -> MP m Text
pTagClose tagname = try $ do
  (t,n) <- pHtmlTag
  case t of
    [TagClose m] | map toLower m == tagname -> return n
    _ -> mzero

pHtmlBlockRaw :: PMonad m => MP m Text
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

pEntity :: PMonad m => MP m (PR Inlines)
pEntity = Const . ch <$> pEntityChar

---

pMathWord :: PMonad m => MP m Text
pMathWord = mconcat <$> (many1 mathChunk)
  where mathChunk = (sym '\\' *> (("\\" <>) . tokToVerbatim <$> anyTok))
                 <|> (tokToVerbatim <$> normalMath)
        normalMath = satisfyTok $ \t ->
                        t /= SPACE && t /= NEWLINE && t /= SYM '$'
        tokToVerbatim t = toksToVerbatim [t]

pMath :: PMonad m => MP m (PR Inlines)
pMath = guardExtension TeX_math *>
  try (do
    sym '$'
    display <- option False (True <$ sym '$')
    raw <- if display
              then notFollowedBy (sym '$') *> pMathDisplay
              else notFollowedBy (space <|> newline) *> pMathInline
    raw' <- pApplyMacros' raw
    let mt = if display then DisplayMath else InlineMath
    return $ Const $ math mt raw')

pApplyMacros' :: Text -> MP m Text
pApplyMacros' = return . id -- TODO

pMathDisplay :: PMonad m => MP m Text
pMathDisplay = try (verbTextTill normal mark)
  where mark   = try $ sym '$' *> sym '$'
        normal = (nonNewline <|> SYM '\n' <$ pEndline)

pMathInline :: PMonad m => MP m Text
pMathInline = try $ do
  words' <- sepBy pMathWord (skipMany1 $ pSp <|> pEndline)
  sym '$'
  let digitTok (SYM d) = isDigit d
      digitTok _       = False
  notFollowedBy $ satisfyTok digitTok
  return $ T.unwords words'
