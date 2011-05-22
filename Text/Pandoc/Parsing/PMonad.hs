{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Parsing.PMonad
where
import Text.Pandoc.Parsing.Types
import Text.Pandoc.Shared
import Text.Parsec hiding (space, newline)
import Control.Applicative ((<$>), (<$), (<*), (*>))
import Data.Traversable (sequenceA)
import Control.Monad
import Control.Monad.Trans
import Data.Text (Text)
import Data.Sequence ((|>), viewr, ViewR(..))

-- | Push parser onto stack of endline parsers.
-- These are applied after a newline within a block.
pushEndline :: PMonad m => P t m () -> P t m ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

-- | Pop parser off stack of endline parsers.
popEndline :: PMonad m => P t m ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

-- | Apply a parser in a context with a specified endline parser.
withEndline :: PMonad m => P t m a -> P t m b -> P t m b
withEndline sep p = pushEndline (() <$ sep) *> p <* popEndline

-- | Push parser onto stack of block separator parsers.
-- These are applied after a newline following a block.
pushBlockSep :: PMonad m => P t m () -> P t m ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

-- | Pop parser off of stack of block separator parsers.
popBlockSep :: PMonad m => P t m ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

-- | Apply a parser in a context with specified block separator parser.
withBlockSep :: PMonad m => P t m a -> P t m b -> P t m b
withBlockSep sep p = pushBlockSep (() <$ sep) *> p <* popBlockSep

-- | Parse a block separator.
pBlockSep :: PMonad m => P t m ()
pBlockSep = try (getState >>= sequenceA . sBlockSep) >> return ()

-- | Run a parser and handle messages.
parseWith :: PMonad m => POptions -> P t m a -> [t] -> m a
parseWith opts p t = do
  res <- runParserT p pstate{ sOptions = opts } "input" t
  case res of
       Right x -> return x
       Left s  -> fail (show s)

-- | Log a message if the log level is appropriate.
logM :: PMonad m => LogLevel -> Text -> P t m ()
logM level msg = do
  logLevel <- getOption optLogLevel
  pos <- getPosition
  when (level >= logLevel) $
     lift $ addMessage $ Message level (Just pos) msg

-- | Parse contents of a file with the specified parser.
parseIncludeFile :: PMonad m
                 => (Text -> [t]) -> FilePath -> P t m a -> P t m a
parseIncludeFile tokenizer f parser = do
  inIncludes <- sIncludes <$> getState
  when (f `elem` inIncludes) $
    error $ "Recursive include in " ++ show f
  -- Keep track of filename so we can avoid recursive includes
  modifyState $ \st -> st{ sIncludes = f : inIncludes }
  old <- getInput
  -- set input and parse
  lift (getFile f) >>= setInput . tokenizer
  res <- parser
  -- put everything back as it was and return results of parsing
  modifyState $ \st -> st{ sIncludes = inIncludes }
  setInput old
  return res



