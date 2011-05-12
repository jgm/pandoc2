{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
   FlexibleInstances #-}
module Text.Pandoc.Parsing
where
import Text.Pandoc.Definition
import Data.String
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
import qualified Data.Foldable as F

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


