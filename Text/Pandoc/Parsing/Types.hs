{-# LANGUAGE FlexibleInstances, ImpredicativeTypes, OverloadedStrings #-}
module Text.Pandoc.Parsing.Types
where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Data.String
import qualified Data.Map as M
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Parsec hiding (space, newline)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Control.Applicative ((<$>))

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

data PMonad m => PState t m =
  PState { sOptions      :: POptions
         , sIncludes     :: [FilePath]
         , sEndline      :: Seq (P t m ())
         , sBlockSep     :: Seq (P t m ())
         , sReferences   :: PReferences
         , sQuoteContext :: Maybe QuoteType
         }

data PReferences =
  PReferences { rLinks      :: M.Map Key Source
              , rNotes      :: M.Map Text (PR Blocks)
              }

nullReferences :: PReferences
nullReferences = PReferences { rLinks = M.empty
                             , rNotes = M.empty }

-- | Default parser state.
pstate :: PMonad m => PState t m
pstate = PState { sOptions      = poptions
                , sIncludes     = []
                , sEndline      = Seq.empty
                , sBlockSep     = Seq.empty
                , sReferences   = nullReferences
                , sQuoteContext = Nothing
                }

getReferences :: PMonad m => P t m PReferences
getReferences = sReferences <$> getState

setReferences :: PMonad m => PReferences -> P t m ()
setReferences refs = modifyState $ \st -> st{ sReferences = refs }

type P t m a = ParsecT [t] (PState t m) m a

data PR a = Const a | Future (PReferences -> a)

instance Show (PR a) where
  show _ = "<PR value>"  -- Show needs to be defined for notFollowedBy'

instance Monoid a => Monoid (PR a) where
  mempty                        = Const mempty
  mappend (Const x)  (Const y)  = Const (mappend x y)
  mappend (Const x)  (Future y) = Future (\s -> mappend x (y s))
  mappend (Future x) (Const y)  = Future (\s -> mappend (x s) y)
  mappend (Future x) (Future y) = Future (\s -> mappend (x s) (y s))

instance IsString (PR Inlines) where
  fromString = Const . fromString

liftResult :: (a -> b) -> PR a -> PR b
liftResult f (Const x)  = Const (f x)
liftResult f (Future g) = Future (f . g)

infix 3 <$$>
(<$$>) :: Monad m => (a -> b) -> m (PR a) -> m (PR b)
(<$$>) = liftM . liftResult

evalResult :: PReferences -> PR a -> a
evalResult _    (Const x)  = x
evalResult refs (Future f) = f refs

finalResult :: PMonad m => PR a -> P t m a
finalResult (Const x)   = return x
finalResult (Future f) = f <$> sReferences <$> getState

-- | Retrieve parser option.
getOption :: PMonad m => (POptions -> a) -> P t m a
getOption opt = opt <$> sOptions <$> getState


