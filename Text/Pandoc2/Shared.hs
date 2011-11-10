{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Text.Pandoc2.Shared where
import Text.Pandoc2.Definition
import Data.Data
import Data.Bits
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

data PExtension = Footnotes
                | TeX_math
                | Delimited_code_blocks
                | Markdown_in_HTML_blocks
                | Fancy_lists
                | Definition_lists
                | Header_identifiers
                | All_symbols_escapable
                | Intraword_underscores
                | Blank_before_blockquote
                | Blank_before_header
                | Significant_bullets
                deriving (Show, Read, Data, Typeable, Enum, Eq)

newtype PExtensions = PExtensions { unPExtensions :: Integer }

noExtensions :: PExtensions
noExtensions = PExtensions 0

allExtensions :: PExtensions
allExtensions = PExtensions $ complement 0

setExtensions :: [PExtension] -> PExtensions
setExtensions =
  foldl (\(PExtensions x) e -> PExtensions (setBit x $ fromEnum e))
    noExtensions

isEnabled :: PExtension -> PExtensions -> Bool
isEnabled ext opts =
  testBit (unPExtensions opts) (fromEnum ext)

isDisabled :: PExtension -> PExtensions -> Bool
isDisabled ext opts = not (isEnabled ext opts)

data HTMLMathMethod = PlainMath
                    | MathML
                    | TeXMath
                    deriving (Show, Read, Eq, Data, Typeable)

data POptions =
  POptions { optLogLevel   :: LogLevel
           , optTabStop    :: Int
           , optExtensions :: PExtensions
           , optSmart      :: Bool   -- ^ Enable smart typography
           , optMathMethod :: HTMLMathMethod
           , optCompact    :: Bool   -- ^ Avoid insignificant whitespace
           }

-- | Default parser options.
poptions :: POptions
poptions = POptions { optLogLevel   = WARNING
                    , optTabStop    = 4
                    , optExtensions = noExtensions
                    , optSmart      = False
                    , optMathMethod = TeXMath
                    , optCompact    = False
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
  case go "M" (map toUpper t') of
       Nothing  -> Nothing
       Just n   -> Just (n, sty)
     where t'  = T.unpack t
           sty = case t' of
                      (c:_) | isLower c -> LowerRoman
                      _                 -> UpperRoman
           go :: Text -> String -> Maybe Int
           go "M" ('M':xs) = fmap (+ 1000) $ go "M" xs
           go "M" xs       = go "CM" xs
           go "CM" ('C':'M':xs) = fmap (+ 900) $ go "C" xs
           go "CM" xs      = go "D" xs
           go "D" ('D':xs) = fmap (+ 500) $ go "D" xs
           go "D" xs       = go "CD" xs
           go "CD" ('C':'D':xs) = fmap (+ 400) $ go "XC" xs
           go "CD" xs      = go "C" xs
           go "C" ('C':xs) = fmap (+ 100) $ go "C" xs
           go "C" xs       = go "XC" xs
           go "XC" ('X':'C':xs) = fmap (+ 90) $ go "X" xs
           go "XC" xs      = go "L" xs
           go "L" ('L':xs) = fmap (+ 50) $ go "L" xs
           go "L" xs       = go "XL" xs
           go "XL" ('X':'L':xs) = fmap (+ 40) $ go "V" xs
           go "XL" xs      = go "X" xs
           go "X" ('X':xs) = fmap (+ 10) $ go "X" xs
           go "X" xs       = go "IX" xs
           go "IX" ('I':'X':xs) = fmap (+ 9) $ go "V" xs
           go "IX" xs      = go "V" xs
           go "V" ('V':xs) = fmap (+ 5) $ go "V" xs
           go "V" xs       = go "IV" xs
           go "IV" ('I':'V':_) = Just 4
           go "IV" xs      = go "I" xs
           go "I" ('I':xs) = fmap (+ 1) $ go "I" xs
           go "I" []       = Just 0
           go _   _        = Nothing
