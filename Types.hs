{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MultiParamTypeClasses #-}
module Types
where
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

newtype Blocks = Blocks { unBlocks :: Seq Block }
                deriving (Data, Ord, Eq, Typeable)

-- We show a Blocks just like [Block].
instance Show Blocks where
  show = show . toList . unBlocks

instance Read Blocks where
  readsPrec n = map (\(x,y) -> (Blocks . fromList $ x, y)) . readsPrec n

