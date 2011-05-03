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
import System.IO (stderr)
import qualified Data.Text.IO as T
import Control.Applicative

data PState = PState {
                  sGetFile  :: FilePath -> P Text
                , sMessages :: Seq Text
                , sLogLevel :: LogLevel
                }

pstate :: PState
pstate = PState { sGetFile  = undefined
                , sMessages = Seq.empty
                , sLogLevel = WARNING
                }

type P a = ParsecT Text PState IO a

instance Stream Text IO Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

showText :: Show a => a -> Text
showText = T.pack . show

logM :: LogLevel -> Text -> P ()
logM level msg = do
  logLevel <- fmap sLogLevel getState
  pos <- getPosition
  let msg' = showText level <> " (line " <>
             showText (sourceLine pos) <> " col " <>
             showText (sourceColumn pos) <> "): " <> msg
  if level >= logLevel
     then modifyState $ \st ->
              st { sMessages = sMessages st |> msg' }
     else return ()

parseWith :: P a -> Text -> IO a
parseWith p t = do
  let p' = do x <- p
              msgs <- fmap sMessages getState
              return (x, msgs)
  res <- runParserT p' pstate "input" t
  case res of
       Left err -> error $ show err
       Right (x, msgs) -> do
                   mapM_ (T.hPutStrLn stderr) $ toList msgs
                   return x

pInline :: P Inlines
pInline = choice [ pSp, pTxt ]

pInlines :: P Inlines
pInlines = mconcat <$> many1 pInline

pSp :: P Inlines
pSp = many1 space *> return sp

pTxt :: P Inlines
pTxt = literal . T.pack <$> many1 letter

