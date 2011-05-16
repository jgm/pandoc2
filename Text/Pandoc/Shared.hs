{-# LANGUAGE DeriveDataTypeable #-}

module Text.Pandoc.Shared where
import Text.Pandoc.Definition
import Data.Data
import Data.Monoid
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B8
import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Data.String
import Data.Text (Text)
import Network.URI ( escapeURIString, isAllowedInURI )
import qualified Data.Text as T
import Data.Generics.Uniplate.Data

data LogLevel = INFO | WARNING | ERROR
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
  POptions { optLogLevel :: LogLevel
           , optTabStop  :: Int
           , optStrict   :: Bool  -- ^ Use standard markdown syntax, no exts.
           , optSmart    :: Bool  -- ^ Smart typography
           }

-- | Default parser options.
poptions :: POptions
poptions = POptions { optLogLevel = WARNING
                    , optTabStop  = 4
                    , optStrict   = False
                    , optSmart    = False
                    }

-- | Concatenate and trim inlines.
toInlines :: [Inlines] -> Inlines
toInlines = trimInlines . mconcat

-- | Remove links from 'Inlines'.
delink :: Inlines -> Inlines
delink = mapItems go
  where go (Link (Label lab) _)            = lab
        go x                               = single x

-- | Escape a URI, converting to UTF-8 octets, then URI encoding them.
escapeURI :: Text -> Text
escapeURI = T.pack . escapeURIString isAllowedInURI .
            B8.unpack . E.encodeUtf8

-- | Version of 'show' that works for any 'IsString' instance.
show' :: (Show a, IsString b) => a -> b
show' = fromString . show

-- | Convert inlines to plain text.
textify :: Inlines -> Text
textify = T.concat . map extractText . universeBi
  where extractText :: Inline -> Text
        extractText (Txt t)   = t
        extractText Sp        = T.singleton ' '
        extractText LineBreak = T.singleton ' '
        extractText _         = T.singleton ' '

