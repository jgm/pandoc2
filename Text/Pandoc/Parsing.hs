{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
   FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
module Text.Pandoc.Parsing
where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Data.Traversable (sequenceA)
import Data.Char (isLetter, isAlphaNum)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import Text.Parsec hiding (space, newline)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Parsec.Pos
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewR(..), viewr, (|>))
import Control.Applicative ((<$>), (<$), (<*), (*>))

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

nonindentSpace :: (PMonad m, Stream [Tok] m Tok)
               => ParsecT [Tok] (PState m) m Int
nonindentSpace = try $ do
  tabstop <- getOption optTabStop
  s' <- many space
  let l = length s'
  if l < tabstop
     then return l
     else unexpected "indentation"

indentSpace :: (PMonad m, Stream [Tok] m Tok) => ParsecT [Tok] (PState m) m ()
indentSpace = try $ do
  tabstop <- getOption optTabStop
  count tabstop space *> return ()

-- | 'sepBy' redefined to include a 'try', so the separator
-- can fail.
sepBy :: Stream s m t
      => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
sepBy p sep = do
  x <- p
  xs <- many $ try (sep *> p)
  return (x:xs)

upto :: Stream s m t => Int -> ParsecT s u m t -> ParsecT s u m [t]
upto n _ | n <= 0 = return []
upto n p = do
  (p >>= (\x -> (x:) <$> upto (n-1) p)) <|> return []

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser
-- fails. It does not consume any input.
notFollowedBy' :: (Stream s m t, Show b)
               => ParsecT s u m b -> ParsecT s u m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

-- | Like 'manyTill', but parses at least one element.
many1Till :: Stream s m t
          => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p q = do
  x <- p
  xs <- manyTill p q
  return (x:xs)

data Result a = Success [Message] a
              | Failure String
              deriving Show

instance Monad Result where
  return x = Success [] x
  fail   s = Failure s
  Failure s    >>= _ = Failure s
  Success ms x >>= f =
    case f x of
         Failure s      -> Failure s
         Success ms' x' -> Success (ms `mappend` ms') x'

class Monad m => PMonad m where
  addMessage :: Message -> m ()
  getFile    :: FilePath -> m Text

instance PMonad Result where
  addMessage m = Success [m] ()
  getFile    f = Success [Message WARNING Nothing $
                    "Skipping include file " <> show' f] mempty

instance PMonad IO where
  addMessage m = liftIO $ hPutStrLn stderr $ show m
  getFile    f = liftIO $ T.readFile f

instance PMonad Maybe where
  addMessage _ = Just ()
  getFile    _ = Nothing

data PMonad m => PState m =
  PState { sOptions      :: POptions
         , sIncludes     :: [FilePath]
         , sEndline      :: Seq (P m ())
         , sBlockSep     :: Seq (P m ())
         , sReferences   :: PReferences
         , sQuoteContext :: Maybe QuoteType
         }

data PReferences =
  PReferences { rLinks      :: M.Map Key Source
              , rNotes      :: M.Map Key Blocks
              }

-- | Default parser state.
pstate :: PMonad m => PState m
pstate = PState { sOptions      = poptions
                , sIncludes     = []
                , sEndline      = Seq.empty
                , sBlockSep     = Seq.empty
                , sReferences   = PReferences { rLinks = M.empty
                                              , rNotes = M.empty }
                , sQuoteContext = Nothing
                }

type P m a = ParsecT [Tok] (PState m) m a

data PR a = Stable a | Variable (PReferences -> a)

instance Monoid a => Monoid (PR a) where
  mempty                            = Stable mempty
  mappend (Stable x)   (Stable y)   = Stable (mappend x y)
  mappend (Stable x)   (Variable y) = Variable (\s -> mappend x (y s))
  mappend (Variable x) (Stable y)   = Variable (\s -> mappend (x s) y)
  mappend (Variable x) (Variable y) = Variable (\s -> mappend (x s) (y s))

liftResult :: (a -> b) -> PR a -> PR b
liftResult f (Stable x)   = Stable (f x)
liftResult f (Variable g) = Variable (f . g)

(<$$>) :: (a -> b) -> PR a -> PR b
(<$$>) = liftResult

finalResult :: PMonad m => PR a -> P m a
finalResult (Stable x)   = return x
finalResult (Variable f) = f <$> sReferences <$> getState

-- | Retrieve parser option.
getOption :: PMonad m => (POptions -> a) -> P m a
getOption opt = opt <$> sOptions <$> getState

-- | Run a parser and handle messages.
parseWith :: PMonad m => POptions -> P m a -> Text -> m a
parseWith opts p t = do
  res <- runParserT p pstate{ sOptions = opts } "input" $ tokenize opts t
  case res of
       Right x -> return x
       Left s  -> fail (show s)

-- | Log a message if the log level is appropriate.
logM :: PMonad m => LogLevel -> Text -> P m ()
logM level msg = do
  logLevel <- getOption optLogLevel
  pos <- getPosition
  when (level >= logLevel) $
     lift $ addMessage $ Message level (Just pos) msg

-- | Parse contents of a file with the specified parser.
parseIncludeFile :: PMonad m => FilePath -> P m a -> P m a
parseIncludeFile f parser = do
  inIncludes <- sIncludes <$> getState
  when (f `elem` inIncludes) $
    error $ "Recursive include in " ++ show f
  -- Keep track of filename so we can avoid recursive includes
  modifyState $ \st -> st{ sIncludes = f : inIncludes }
  old <- getInput
  -- set input and parse
  opts <- sOptions <$> getState
  lift (getFile f) >>= setInput . tokenize opts
  res <- parser
  -- put everything back as it was and return results of parsing
  modifyState $ \st -> st{ sIncludes = inIncludes }
  setInput old
  return res

-- | Push parser onto stack of endline parsers.
-- These are applied after a newline within a block.
pushEndline :: PMonad m => P m () -> P m ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

-- | Pop parser off stack of endline parsers.
popEndline :: PMonad m => P m ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

-- | Apply a parser in a context with a specified endline parser.
withEndline :: PMonad m => P m a -> P m b -> P m b
withEndline sep p = pushEndline (() <$ sep) *> p <* popEndline

-- | Push parser onto stack of block separator parsers.
-- These are applied after a newline following a block.
pushBlockSep :: PMonad m => P m () -> P m ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

-- | Pop parser off of stack of block separator parsers.
popBlockSep :: PMonad m => P m ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

-- | Apply a parser in a context with specified block separator parser.
withBlockSep :: PMonad m => P m a -> P m b -> P m b
withBlockSep sep p = pushBlockSep (() <$ sep) *> p <* popBlockSep

-- | Parse a block separator.
pBlockSep :: PMonad m => P m ()
pBlockSep = try (getState >>= sequenceA . sBlockSep) >> return ()

-- | Parse multiple block-separating line breaks. Return number of
-- newlines parsed.
pNewlines :: PMonad m => P m Int
pNewlines = length <$> many1 pNewline

-- | Parse a block-separating line break.
pNewline :: PMonad m => P m ()
pNewline = try $ spnl *> pBlockSep

-- | Parse a line break within a block, including characters
-- at the beginning of the next line that are part of the block
-- context. Return a Space.
pEndline :: PMonad m => P m (PR Inlines)
pEndline = try $
  newline *> (getState >>= sequenceA . sEndline) *> sps *>
  lookAhead nonNewline *> return (Stable $ single Sp)

-- | Parses line-ending spaces, if present, and optionally
-- an endline followed by any spaces at the beginning of
-- the next line.
spOptNl :: PMonad m => P m ()
spOptNl = try $ sps <* optional (pEndline <* sps)

-- | Parse a verbatim line of text, not including the newline.
verbLine :: PMonad m => P m Text
verbLine = cleanup . toksToVerbatim <$> many nonNewline
  where cleanup t = if T.all iswhite t then T.empty else t
        iswhite c = c == ' ' || c == '\t'

-- | Fail unless in column 1.
pInColumn1 :: Monad m => P m ()
pInColumn1 = do
  pos <- getPosition
  guard $ sourceColumn pos == 1

pEntityChar :: PMonad m => P m Char
pEntityChar = try $ do
  sym '&'
  x <- textTill nonSpace (sym ';')
  case lookupEntity (T.unpack x) of
       Just c   -> return c
       _        -> mzero
-- quote parsers

pQuotedWith :: PMonad m => QuoteType -> P m (PR Inlines) -> P m (PR Inlines)
pQuotedWith qt ins = withQuoteContext qt $ do
  ils <- mconcat <$> (many1Till ins (quoteEnd qt))
  return $ (single . Quoted qt . trimInlines) <$$> ils

withQuoteContext :: PMonad m => QuoteType -> P m (PR Inlines) -> P m (PR Inlines)
withQuoteContext qt ins = try $ do
  oldContext <- sQuoteContext <$> getState
  modifyState $ \st -> st{ sQuoteContext = Just qt }
  result <- ins
  modifyState $ \st -> st{ sQuoteContext = oldContext }
  return result

failIfInQuoteContext :: PMonad m => QuoteType -> P m ()
failIfInQuoteContext qt =
  sQuoteContext <$> getState >>= guard . (/= Just qt)

charOrRef :: PMonad m => (Char -> Bool) -> P m Char
charOrRef f = try $ do
  SYM c <- satisfyTok isSymTok <|> SYM <$> pEntityChar
  guard $ f c
  return c

quoteStart :: PMonad m => QuoteType -> P m ()
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

quoteEnd :: PMonad m => QuoteType -> P m ()
quoteEnd SingleQuoted =
  charOrRef (\c -> c == '\'' || c == '\8217') *> notFollowedBy wordTok
quoteEnd DoubleQuoted = 
  charOrRef (\c -> c == '"' || c == '\8221') *> return ()
