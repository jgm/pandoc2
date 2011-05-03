{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Definition
where
import Data.Sequence hiding (null)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import Data.List (intersperse)
import Data.Foldable (toList)
import Data.Generics
import Data.String
import Text.Parsec
import Control.Monad.Identity (Identity)

data Block = Para Inlines
           | Plain Inlines
           | Quote Blocks
           | Code Attr Text
           | OrderedList [Blocks]
           | BulletList [Blocks]
           | RawBlock Format Text
           deriving (Show, Read, Data, Ord, Eq, Typeable)

data Inline = Txt Text
            | Sp
            | Emph Inlines
            | Strong Inlines
            | Image Label Title Source
            | Link Label Title Source
            | Verbatim Attr Text
            | LineBreak
            | RawInline Format Text
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

nullAttr :: Attr
nullAttr = Attr "" [] []

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
                          (Sp, LineBreak)    -> xs' |> LineBreak
                          _                  -> xs' |> x |> y

instance IsString Inlines where
  fromString = txt . T.pack

newtype Blocks = Blocks { unBlocks :: Seq Block }
                deriving (Data, Ord, Eq, Typeable, Monoid)

-- We show a Blocks just like [Block].
instance Show Blocks where
  show = show . toList . unBlocks

instance Read Blocks where
  readsPrec n = map (\(x,y) -> (Blocks . fromList $ x, y)) . readsPrec n

-- Pandoc builder DSL

inline :: Inline -> Inlines
inline = Inlines . singleton

-- | Convert a 'Text' to 'Inlines', treating interword spaces as 'Sp's.
-- If you want a 'Str' with literal spaces, use 'literal'.
txt :: T.Text -> Inlines
txt = Inlines . fromList . intersperse Sp . map Txt . T.words

literal :: Text -> Inlines
literal = inline . Txt

sp :: Inlines
sp = inline Sp

emph :: Inlines -> Inlines
emph = inline . Emph

strong :: Inlines -> Inlines
strong = inline . Strong

link :: Inlines -> Text -> Text -> Inlines
link lab tit src = inline $ Link (Label lab) (Title tit) (Source src)

image :: Inlines -> Text -> Text -> Inlines
image lab tit src = inline $ Image (Label lab) (Title tit) (Source src)

verbatim :: Text -> Inlines
verbatim = verbatimAttr nullAttr

verbatimAttr :: Attr -> Text -> Inlines
verbatimAttr attr = inline . Verbatim attr

lineBreak :: Inlines
lineBreak = inline LineBreak

rawInline :: Format -> Text -> Inlines
rawInline f = inline . RawInline f

block :: Block -> Blocks
block = Blocks . singleton

para :: Inlines -> Blocks
para = block . Para

plain :: Inlines -> Blocks
plain = block . Plain

quote :: Blocks -> Blocks
quote = block . Quote

codeAttr :: Attr -> Text -> Blocks
codeAttr attr = block . Code attr

code :: Text -> Blocks
code = codeAttr nullAttr

orderedList :: [Blocks] -> Blocks
orderedList = block . OrderedList

bulletList :: [Blocks] -> Blocks
bulletList = block . BulletList

rawBlock :: Format -> Text -> Blocks
rawBlock f = block . RawBlock f

-- Generics:

bottomUp f = everywhere (mkT f)

