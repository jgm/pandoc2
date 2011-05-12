module Text.Pandoc.Shared where
import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Data.Text (Text)
import qualified Data.Text as T

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

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


