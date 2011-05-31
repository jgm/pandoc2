{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Text.Pandoc2.Parsing.TextTok
where
import Text.Pandoc2.Definition
import Text.Pandoc2.Shared
import Text.Pandoc2.Parsing.Types
import Text.Pandoc2.Parsing.PMonad
import Text.Pandoc2.Parsing.Generic
import Control.Monad (guard, mzero)
import Data.Monoid
import Text.HTML.TagSoup.Entity (lookupEntity)
import Data.Char (isLetter, isAlphaNum)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec hiding (space, newline)
import Text.Parsec.Pos
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Data.Traversable (sequenceA)

data Tok = WORD Text    -- ^ string of alphanumerics
         | SPACE        -- ^ a space
         | NEWLINE      -- ^ lf, cr, crlf, or eof
         | SYM Char     -- ^ a non-alphanumeric
         deriving (Show, Eq)

tokenize :: POptions -> Text -> [Tok]
tokenize opts = tokenize' (optTabStop opts) 0

tokenize' :: Int -> Int -> Text -> [Tok]
tokenize' stop pos t =
  case T.uncons t of
       Nothing     -> [NEWLINE]
       Just (c, _) | isLetter c ->
         case T.span isAlphaNum t of
              (x,y) -> WORD x : tokenize'' (pos + T.length x) y
       Just ('\t', t') ->
              let n = stop - pos `mod` stop
              in  replicate n SPACE ++ tokenize'' (pos + n) t'
       Just (' ', t')  -> SPACE : tokenize'' (pos + 1) t'
       Just ('\r',t')  ->
              case T.uncons t' of
                   Just ('\n',t'') -> NEWLINE : tokenize'' 0 t''
                   _               -> NEWLINE : tokenize'' 0 t'
       Just ('\n',t')  -> NEWLINE : tokenize'' 0 t'
       Just (c, t')   -> SYM c : tokenize'' (pos + 1) t'
    where tokenize''    = tokenize' stop

toksToText :: [Tok] -> Text
toksToText = T.concat . go
  where go :: [Tok] -> [Text]
        go (WORD x : ts) = x : go ts
        go (SPACE  : ts) = T.singleton ' ' : go ts
        go (NEWLINE: ts) = T.singleton ' ' : go ts
        go (SYM '\\' : SYM c : ts) = T.singleton c : go ts
        go (SYM c  : ts) = T.singleton c   : go ts
        go []            = []

toksToVerbatim :: [Tok] -> Text
toksToVerbatim = T.concat . go
  where go :: [Tok] -> [Text]
        go (WORD x : ts) = x : go ts
        go (SPACE  : ts) = T.singleton ' ' : go ts
        go (NEWLINE: ts) = T.singleton '\n' : go ts
        go (SYM c  : ts) = T.singleton c   : go ts
        go []            = []

showTok :: Tok -> String
showTok (WORD t) = show t
showTok SPACE    = "space"
showTok NEWLINE  = "newline"
showTok (SYM c)  = show c

updatePosTok :: SourcePos -> Tok -> s -> SourcePos
updatePosTok pos (WORD t) _ = incSourceColumn pos (T.length t)
updatePosTok pos SPACE    _ = incSourceColumn pos 1
updatePosTok pos NEWLINE  _ = updatePosChar pos '\n'
updatePosTok pos (SYM _)  _ = incSourceColumn pos 1

isSymTok :: Tok -> Bool
isSymTok (SYM _) = True
isSymTok _ = False

isWordTok :: Tok -> Bool
isWordTok (WORD _) = True
isWordTok _ = False

wordTok :: Stream s m Tok => ParsecT s u m Tok
wordTok = satisfyTok isWordTok

anyTok :: Stream s m Tok => ParsecT s u m Tok
anyTok = tokenPrim showTok updatePosTok Just

satisfyTok :: Stream s m Tok => (Tok -> Bool) -> ParsecT s u m Tok
satisfyTok f = tokenPrim showTok updatePosTok $ \t -> if f t
                                                         then Just t
                                                         else Nothing

sym :: Stream s m Tok => Char -> ParsecT s u m Tok
sym c = satisfyTok (== SYM c)

textTill :: Stream s m Tok
         => ParsecT s u m Tok -> ParsecT s u m a -> ParsecT s u m Text
textTill p end = toksToText <$> manyTill p end

text1Till :: Stream s m Tok
         => ParsecT s u m Tok -> ParsecT s u m a -> ParsecT s u m Text
text1Till p end = try $ do
  x <- p
  xs <- manyTill p end
  return $ toksToText (x:xs)

verbTextTill :: Stream s m Tok
         => ParsecT s u m Tok -> ParsecT s u m a -> ParsecT s u m Text
verbTextTill p end = toksToVerbatim <$> manyTill p end

space :: Stream s m Tok => ParsecT s u m Tok
space = satisfyTok (== SPACE)

newline :: Stream s m Tok => ParsecT s u m Tok
newline = satisfyTok (== NEWLINE)

nonSpace :: Stream s m Tok => ParsecT s u m Tok
nonSpace = satisfyTok (\t -> t /= SPACE && t /= NEWLINE)

nonNewline :: Stream s m Tok => ParsecT s u m Tok
nonNewline = satisfyTok (\t -> t /= NEWLINE)

sps :: Stream s m Tok => ParsecT s u m ()
sps = skipMany space

spnl :: Stream s m Tok => ParsecT s u m Tok
spnl = sps *> newline

eol :: Stream s m Tok => ParsecT s u m ()
eol = () <$ (sps *> lookAhead newline)

nonindentSpace :: PMonad m => P Tok m Int
nonindentSpace = try $ do
  -- tabstop <- getOption optTabStop
  let tabstop = 4
  s' <- many space
  let l = length s'
  if l < tabstop
     then return l
     else unexpected "indentation"

indentSpace :: PMonad m => P Tok m ()
indentSpace = try $ do
  tabstop <- getOption optTabStop
  count tabstop space *> return ()

-- | Parse multiple block-separating line breaks. Return number of
-- newlines parsed.
pNewlines :: PMonad m => P Tok m Int
pNewlines = length <$> many1 pNewline <* notFollowedBy spnl

-- | Parse a block-separating line break.
pNewline :: PMonad m => P Tok m ()
pNewline = try $ spnl *> pBlockSep

-- | Parse a line break within a block, including characters
-- at the beginning of the next line that are part of the block
-- context. Return a Space.
pEndline :: PMonad m => P Tok m (PR Inlines)
pEndline = try $
  newline *> (getState >>= sequenceA . sEndline) *> sps *>
  lookAhead nonNewline *> return (Const $ single Sp)

-- | Parses line-ending spaces, if present, and optionally
-- an endline followed by any spaces at the beginning of
-- the next line.
spOptNl :: PMonad m => P Tok m ()
spOptNl = try $ sps <* optional (pEndline <* sps)

-- | Parse a verbatim line of text, not including the newline.
verbLine :: PMonad m => P Tok m Text
verbLine = cleanup . toksToVerbatim <$> many nonNewline
  where cleanup t = if T.all iswhite t then T.empty else t
        iswhite c = c == ' ' || c == '\t'

pEntityChar :: PMonad m => P Tok m Char
pEntityChar = try $ do
  sym '&'
  x <- textTill nonSpace (sym ';')
  case lookupEntity (T.unpack x) of
       Just c   -> return c
       _        -> mzero
-- quote parsers

pQuotedWith :: PMonad m => QuoteType -> P Tok m (PR Inlines) -> P Tok m (PR Inlines)
pQuotedWith qt ins = (single . Quoted qt . trimInlines) <$$>
  (withQuoteContext qt $ mconcat <$> (many1Till ins (quoteEnd qt)))

withQuoteContext :: PMonad m => QuoteType -> P Tok m (PR Inlines) -> P Tok m (PR Inlines)
withQuoteContext qt ins = try $ do
  oldContext <- sQuoteContext <$> getState
  modifyState $ \st -> st{ sQuoteContext = Just qt }
  result <- ins
  modifyState $ \st -> st{ sQuoteContext = oldContext }
  return result

failIfInQuoteContext :: PMonad m => QuoteType -> P Tok m ()
failIfInQuoteContext qt =
  sQuoteContext <$> getState >>= guard . (/= Just qt)

charOrRef :: PMonad m => (Char -> Bool) -> P Tok m Char
charOrRef f = try $ do
  SYM c <- satisfyTok isSymTok <|> SYM <$> pEntityChar
  guard $ f c
  return c

quoteStart :: PMonad m => QuoteType -> P Tok m ()
quoteStart SingleQuoted = do
  failIfInQuoteContext SingleQuoted
  try $ do charOrRef (\c -> c == '\'' || c == '\8216')
           notFollowedBy $ satisfyTok (==SPACE) <|> satisfyTok (==NEWLINE)
           notFollowedBy $ charOrRef (\c -> c == ')' || c == '!' || c == ']' ||
                             c == ',' || c == '.' || c == ';' || c == ':' ||
                             c == '-' || c == '?')
           return ()
quoteStart DoubleQuoted = do
  failIfInQuoteContext DoubleQuoted
  try $ do charOrRef (\c -> c == '"' || c == '\8220')
           notFollowedBy $ satisfyTok (==SPACE) <|> satisfyTok (==NEWLINE)

quoteEnd :: PMonad m => QuoteType -> P Tok m ()
quoteEnd SingleQuoted =
  charOrRef (\c -> c == '\'' || c == '\8217') *> notFollowedBy wordTok
quoteEnd DoubleQuoted = 
  charOrRef (\c -> c == '"' || c == '\8221') *> return ()
