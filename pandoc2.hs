{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MultiParamTypeClasses #-}

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

data Inline = Txt Text
            | Sp
            | Emph Inlines
            | Strong Inlines
            | SmallCaps Inlines
            | Underline Inlines
            | Strike Inlines
            | Superscript Inlines
            | Subscript Inlines
            | Image Label Title Source
            | Link Label Title Source
            | Verbatim Attr Text
            | LineBreak
            | RawInline Format Text
            | Note
            deriving (Show, Read, Data, Ord, Eq, Typeable)

newtype Label = Label Inlines
              deriving (Show, Read, Data, Ord, Eq, Typeable)

newtype Title = Title Text
              deriving (Show, Read, Data, Ord, Eq, Typeable)

newtype Source = Source Text
              deriving (Show, Read, Data, Ord, Eq, Typeable)

newtype Format = Format Text
               deriving (Show, Read, Data, Typeable)

instance Eq Format where
  Format x == Format y = T.toUpper x == T.toUpper y

instance Ord Format where
  Format x `compare` Format y = T.toUpper x `compare` T.toUpper y

data Attr = Attr { attrId      :: Text
                 , attrClasses :: [Text]
                 , attrKeyVals :: [(Text, Text)] }
                 deriving (Show, Read, Data, Ord, Eq, Typeable)

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

newtype Inlines = Inlines { unInlines :: Seq Inline }
                deriving (Data, Ord, Eq, Typeable)

-- We show an Inlines just like [Inline].
instance Show Inlines where
  show = show . toList . unInlines

instance Read Inlines where
  readsPrec n = map (\(x,y) -> (Inlines . fromList $ x, y)) . readsPrec n

instance Monoid Inlines where
  mempty = Inlines mempty
  (Inlines xs) `mappend` (Inlines ys) =
    case (viewr xs, viewl ys) of
      (EmptyR, _) -> Inlines ys
      (_, EmptyL) -> Inlines xs
      (xs' :> x, y :< ys') -> Inlines (meld `mappend` ys')
        where meld = case (x, y) of
                          (Sp, Sp)           -> xs' |> Sp
                          (Txt t1, Txt t2)   -> xs' |> Txt (t1 <> t2)
                          (Emph i1, Emph i2) -> xs' |> Emph (i1 <> i2)
                          (Strong i1, Strong i2) -> xs' |> Strong (i1 <> i2)
                          (SmallCaps i1, SmallCaps i2) ->
                                                xs' |> SmallCaps (i1 <> i2)
                          (Underline i1, Underline i2) ->
                                                xs' |> Underline (i1 <> i2)
                          (Strike i1, Strike i2) -> xs' |> Strike (i1 <> i2)
                          (Superscript i1, Superscript i2) ->
                                                xs' |> Superscript (i1 <> i2)
                          (Subscript i1, Subscript i2) ->
                                                xs' |> Subscript (i1 <> i2)
                          (Sp, LineBreak)    -> xs' |> LineBreak
                          _                  -> xs' |> x |> y

newtype Blocks = Blocks { unBlocks :: Seq Block }
                deriving (Data, Ord, Eq, Typeable)

-- We show a Blocks just like [Block].
instance Show Blocks where
  show = show . toList . unBlocks

instance Read Blocks where
  readsPrec n = map (\(x,y) -> (Blocks . fromList $ x, y)) . readsPrec n

data Block = Para Inlines
           deriving (Show, Read, Data, Ord, Eq, Typeable)

-- | Convert a 'Text' to 'Inlines', treating interword spaces as 'Sp's.
-- If you want a 'Str' with literal spaces, use 'literalText'.
txt :: T.Text -> Inlines
txt = Inlines . fromList . intersperse Sp . map Txt . T.words

literalText :: Text -> Inlines
literalText = Inlines . singleton . Txt

emph :: Inlines -> Inlines
emph = Inlines . singleton . Emph

link :: Inlines -> Text -> Text -> Inlines
link lab tit src = Inlines $ singleton $ Link (Label lab) (Title tit) (Source src)

sp :: Inlines
sp = Inlines . singleton $ Sp

-- just testing generics:

bottomUp f = everywhere (mkT f)

gentest :: Inline -> Inline
gentest (Txt t) = Txt $ T.toUpper t
gentest x = x

gentest2 :: Title -> Title
gentest2 (Title x) = (Title "")


-- awkward: still hard to remove an item from a list and replace it with
-- several
gentest3 :: Seq Inline -> Seq Inline
gentest3 x | Data.Sequence.take 1 x == fromList [Txt "hi"] = Txt "H" <| Sp <| Txt "I" <| Data.Sequence.drop 1 x
gentest3 x = x

-----

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

