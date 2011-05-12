{-# LANGUAGE DeriveDataTypeable #-}

module Text.Pandoc.Shared where
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import qualified Data.Foldable as F
import Data.Data
import Data.Monoid
import Data.Sequence as Seq
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B8
import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Data.String
import Data.Text (Text)
import Network.URI ( escapeURIString, isAllowedInURI )
import qualified Data.Text as T

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read, Data, Typeable)

data Message = Message LogLevel (Maybe SourcePos) Text

instance Show Message where
  show (Message level pos t) = show level ++ " " ++ T.unpack t ++
                               showSourcePos pos
    where showSourcePos Nothing = ""
          showSourcePos (Just p) = " (line " ++
             show (sourceLine p) ++ " col " ++
             show (sourceColumn p) ++ ")"

data POptions =
  POptions { optLogLevel  :: LogLevel
           }

-- | Default parser options.
poptions :: POptions
poptions = POptions { optLogLevel  = WARNING
                    }

-- | Trim leading and trailing Sp (spaces) from an Inlines.
trimInlines :: Inlines -> Inlines
trimInlines (Inlines ils) = Inlines $ dropWhileL (== Sp) $
                            dropWhileR (== Sp) $ ils

-- | Concatenate and trim inlines.
toInlines :: [Inlines] -> Inlines
toInlines = trimInlines . mconcat

-- | Remove links from 'Inlines'.
delink :: Inlines -> Inlines
delink = Inlines . F.foldMap (unInlines . go) . unInlines
  where go (Link _ (Ref { fallback = f })) = f
        go (Link (Label lab) _)            = lab
        go x                               = inline x

-- | Escape a URI, converting to UTF-8 octets, then URI encoding them.
escapeURI :: Text -> Text
escapeURI = T.pack . escapeURIString isAllowedInURI .
            B8.unpack . E.encodeUtf8

-- | Version of 'show' that works for any 'IsString' instance.
show' :: (Show a, IsString b) => a -> b
show' = fromString . show

-- | Convert tabs to spaces.
convertTabs :: Int -> Text -> Text
convertTabs tabstop = T.unlines . Prelude.map convertTabL . T.lines
  where convertTabL l =
          case T.break (=='\t') l of
                (_,x) | T.null x -> l
                (x,y) -> x <> ss <> convertTabL (T.tail y)
                          where ss = T.replicate (tabstop - (n `mod` tabstop)) s
                                s  = T.pack " "
                                n  = T.length x


