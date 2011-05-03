{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,  GeneralizedNewtypeDeriving #-}
module Parser
where
import Definition
import Data.Sequence as Seq
import Data.Monoid
import Data.Traversable
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

{-
 - Processing model:
 - sEndline - Seq of parsers;
 -    when we hit a Quote block, add "optional > "
 -    when we hit a list item, add "optional (spaces notfollowedby listmarker)"
 -    parse in sequence, then return Sp
 - sBlockSep - optional blanklines (dep. on tight/loose) >> Seq of parsers;
 -    when we hit a Quote block, add "> "
 -    when we hit a list item, add "indentspace or blankline"
 - use Data.Traversable.sequence to run all the parsers
 -
 - when we hit a new list item or exit the quote, pop stuff off of these.
-}


data PState = PState {
                  sGetFile    :: FilePath -> P Text
                , sMessages   :: Seq Text
                , sLogLevel   :: LogLevel
                , sEndline    :: Seq (P ())
                , sBlockSep   :: Seq (P ())
                }

pstate :: PState
pstate = PState { sGetFile  = undefined
                , sMessages = Seq.empty
                , sLogLevel = WARNING
                , sEndline  = Seq.empty
                , sBlockSep = Seq.empty
                }

type P a = ParsecT Text PState IO a

instance Stream Text IO Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

pushEndline :: P () -> P ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

popEndline :: P ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

pushBlockSep :: P () -> P ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

popBlockSep :: P ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

endline :: P Inlines
endline = sp <$ try (newline *> getState >>= sequenceA . sEndline)

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

