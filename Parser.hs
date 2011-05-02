{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MultiParamTypeClasses #-}
module Parser
where
import Types
import Builder
import Generic
import Data.Sequence hiding (null)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import Data.List (intersperse)
import Data.Foldable (toList)
import Data.Generics
import Text.Parsec
import Control.Monad.Identity (Identity)

data PState = PState

type P a = ParsecT Text PState IO a

instance Stream Text IO Char where
  uncons = return . T.uncons

parseWith :: P a -> Text -> IO a
parseWith p t = do
  res <- runParserT p PState "input" t
  case res of 
       Left err -> error $ show err
       Right x  -> return x

