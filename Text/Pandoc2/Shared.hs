{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Text.Pandoc2.Shared where
import Text.Pandoc2.Definition
import Data.Data
import Data.Char
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
        extractText (Txt t)    = t
        extractText (Verbatim _ t) = t
        extractText (Math _ t) = t
        extractText (Quoted DoubleQuoted ils) = "\"" <> textify ils <> "\""
        extractText (Quoted SingleQuoted ils) = "'"  <> textify ils <> "'"
        extractText Sp         = T.singleton ' '
        extractText LineBreak  = T.singleton ' '
        extractText _          = mempty

inlinesToIdentifier :: Inlines -> Text
inlinesToIdentifier = T.dropWhile (not . isAlpha)
  . T.intercalate "-"
  . T.words
  . T.map (nbspToSp . toLower)
  . T.filter (\c -> isLetter c || isDigit c || c `elem` "_-. ")
  . textify
 where nbspToSp '\160'     =  ' '
       nbspToSp x          =  x

fromRoman :: Text -> Maybe (Int, ListNumberStyle)
fromRoman t =
  case toRoman' "M" (map toUpper t') of
       Nothing  -> Nothing
       Just n   -> Just (n, sty)
     where t'  = T.unpack t
           sty = case t' of
                      (c:_) | isLower c -> LowerRoman
                      _                 -> UpperRoman
           toRoman' :: String -> String -> Maybe Int
           toRoman' _   (c:_) | not (c == 'M' || c == 'C' || c == 'D' ||
                                     c == 'L' || c == 'X' || c == 'V' ||
                                     c == 'I') = Nothing
           toRoman' "M" ('M':xs) = fmap (+ 1000) $ toRoman' "M" xs
           toRoman' "M" xs       = toRoman' "CM" xs
           toRoman' "CM" ('C':'M':xs) = fmap (+ 900) $ toRoman' "C" xs
           toRoman' "CM" xs      = toRoman' "D" xs
           toRoman' "D" ('D':xs) = fmap (+ 500) $ toRoman' "D" xs
           toRoman' "D" xs       = toRoman' "CD" xs
           toRoman' "CD" ('C':'D':xs) = fmap (+ 400) $ toRoman' "XC" xs
           toRoman' "CD" xs      = toRoman' "C" xs
           toRoman' "C" ('C':xs) = fmap (+ 100) $ toRoman' "C" xs
           toRoman' "C" xs       = toRoman' "XC" xs
           toRoman' "XC" ('X':'C':xs) = fmap (+ 90) $ toRoman' "X" xs
           toRoman' "XC" xs      = toRoman' "L" xs
           toRoman' "L" ('L':xs) = fmap (+ 50) $ toRoman' "L" xs
           toRoman' "L" xs       = toRoman' "XL" xs
           toRoman' "XL" ('X':'L':xs) = fmap (+ 40) $ toRoman' "V" xs
           toRoman' "XL" xs      = toRoman' "X" xs
           toRoman' "X" ('X':xs) = fmap (+ 10) $ toRoman' "X" xs
           toRoman' "X" xs       = toRoman' "IX" xs
           toRoman' "IX" ('I':'X':xs) = fmap (+ 9) $ toRoman' "V" xs
           toRoman' "IX" xs      = toRoman' "V" xs
           toRoman' "V" ('V':xs) = fmap (+ 10) $ toRoman' "V" xs
           toRoman' "V" xs       = toRoman' "IV" xs
           toRoman' "IV" ('I':'V':_) = Just 4
           toRoman' "IV" xs      = toRoman' "I" xs
           toRoman' "I" ('I':xs) = fmap (+ 1) $ toRoman' "I" xs
           toRoman' "I" []       = Just 0
           toRoman' _   _        = Nothing
