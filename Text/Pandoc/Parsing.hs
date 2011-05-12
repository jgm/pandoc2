{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
   FlexibleInstances, OverloadedStrings #-}
module Text.Pandoc.Parsing
where
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Data.String
import Data.Traversable (sequenceA)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import Text.Parsec
import Data.Sequence as Seq
import Control.Applicative ((<$>), (<$), (<*), (*>))

instance Monad m => Stream Text m Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

data Message = Message LogLevel SourcePos Text

instance Show Message where
  show (Message level pos t) = show level ++ " (line " ++
             show (sourceLine pos) ++ " col " ++
             show (sourceColumn pos) ++ "): " ++ T.unpack t

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
  getFile    f = Failure $ "Cannot include file " ++ show f

instance PMonad IO where
  addMessage m = liftIO $ hPutStrLn stderr $ show m
  getFile    f = liftIO $ T.readFile f

instance PMonad Maybe where
  addMessage _ = Just ()
  getFile    _ = Nothing

data POptions =
  POptions { optLogLevel  :: LogLevel
           }

-- | Default parser options.
poptions :: POptions
poptions = POptions { optLogLevel  = WARNING
                    }

data PMonad m => PState m =
  PState { sOptions    :: POptions
         , sIncludes   :: [FilePath]
         , sEndline    :: Seq (P m ())
         , sBlockSep   :: Seq (P m ())
         , sReferences :: M.Map Key Source
         }

-- | Default parser state.
pstate :: PMonad m => PState m
pstate = PState { sOptions    = poptions
                , sIncludes   = []
                , sEndline    = Seq.empty
                , sBlockSep   = Seq.empty
                , sReferences = M.empty
                }

type P m a = ParsecT Text (PState m) m a

-- | Retrieve parser option.
getOption :: PMonad m => (POptions -> a) -> P m a
getOption opt = opt <$> sOptions <$> getState

-- | Version of 'show' that works for any 'IsString' instance.
show' :: (Show a, IsString b) => a -> b
show' = fromString . show

-- | Run a parser and handle messages.
parseWith :: PMonad m => POptions -> P m a -> Text -> m a
parseWith opts p t = do
  res <- runParserT p pstate{ sOptions = opts } "input" t
  case res of
       Right x -> return x
       Left s  -> fail (show s)

-- | Log a message if the log level is appropriate.
logM :: PMonad m => LogLevel -> Text -> P m ()
logM level msg = do
  logLevel <- getOption optLogLevel
  pos <- getPosition
  when (level >= logLevel) $
     lift $ addMessage $ Message level pos msg

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
  lift (getFile f) >>= setInput
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
pNewlines = Prelude.length <$> many1 pNewline

-- | Parse a block-separating line break.
pNewline :: PMonad m => P m ()
pNewline = try $ spnl *> pBlockSep

-- | Parse a line break within a block, including characters
-- at the beginning of the next line that are part of the block
-- context. Return a Space.
pEndline :: PMonad m => P m Inlines
pEndline = try $
  nl *> (getState >>= sequenceA . sEndline) *> skipMany spaceChar *>
  lookAhead nonnl *> return (inline Sp)

-- | Parse a non-newline character.
nonnl :: Monad m => P m Char
nonnl = satisfy $ \c -> c /= '\n' && c /= '\r'

-- | Skip 0 or more spaces or tabs.
sps :: Monad m => P m ()
sps = skipMany spaceChar

-- | Parse a newline (CR, CRLF, LF).
nl :: Monad m => P m Char
nl = char '\n' <|> (char '\r' <* option '\n' (char '\n'))

-- | Skip any spaces, then parse a newline.
spnl :: Monad m => P m ()
spnl = try $ sps <* nl

-- | Succeeds if we're at the end of a line (skipping
-- line-ending spaces if present). Does not parse the newline.
eol :: Monad m => P m ()
eol = sps *> lookAhead (() <$ nl <|> eof)

-- | Parses line-ending spaces, if present, and optionally
-- an endline followed by any spaces at the beginning of
-- the next line.
spOptNl :: PMonad m => P m ()
spOptNl = try $ sps <* optional (pEndline <* sps)

-- | Parses a space or tab.
spaceChar :: Monad m => P m Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- Parses anything other than a space, tab, CR, or LF.
nonSpaceChar :: Monad m => P m Char
nonSpaceChar = satisfy  $ \c ->
  c /= ' ' && c /= '\n' && c /= '\r' && c /= '\t'
