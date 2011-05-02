{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,  GeneralizedNewtypeDeriving #-}
module Parser
where
import Types
import Builder
import Generic
import Data.Sequence as Seq
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import Data.List (intersperse)
import Data.Foldable (toList)
import Data.Generics
import Text.Parsec
import Control.Monad.Identity (Identity)
import Control.Monad
import System.FilePath
import qualified Data.Text.IO as T

data PState = PState {
                  pGetFile  :: FilePath -> P Text
                , pMessages :: Seq Text
                , pLogLevel :: LogLevel
                }

pstate :: PState
pstate = PState { pGetFile  = undefined
                , pMessages = Seq.empty
                , pLogLevel = WARNING
                }

type P a = ParsecT Text PState IO a

instance Stream Text IO Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

logM :: LogLevel -> Text -> P ()
logM level msg = do
  logLevel <- fmap pLogLevel getState
  pos <- getPosition
  let msg' = T.pack (show level ++ " " ++ show pos ++ " ") `mappend` msg
  if level >= logLevel
     then modifyState $ \st ->
              st { pMessages = pMessages st |> msg' }
     else return ()

parseWith :: P a -> Text -> IO a
parseWith p t = do
  let p' = do x <- p
              msgs <- fmap pMessages getState
              return (x, msgs)
  res <- runParserT p' pstate "input" t
  case res of
       Left err -> error $ show err
       Right (x, msgs) -> do
                   mapM_ T.putStrLn $ toList msgs
                   return x

