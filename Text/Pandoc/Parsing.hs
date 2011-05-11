{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
   FlexibleInstances #-}
module Text.Pandoc.Parsing
where
import Text.Pandoc.Definition
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
  Success ms x >>= f = case f x of
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

data PMonad m => PState m =
         PState { sInInclude  :: [FilePath]
                , sMessages   :: Seq Message
                , sLogLevel   :: LogLevel
                , sEndline    :: Seq (P m ())
                , sBlockSep   :: Seq (P m ())
                , sReferences :: M.Map Key Source
                }

pstate :: PMonad m => PState m
pstate = PState { sInInclude  = []
                , sMessages   = Seq.empty
                , sLogLevel   = WARNING
                , sEndline    = Seq.empty
                , sBlockSep   = Seq.empty
                , sReferences = M.empty
                }

type P m a = ParsecT Text (PState m) m a


