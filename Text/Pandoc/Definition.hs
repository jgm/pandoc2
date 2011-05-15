{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Text.Pandoc.Definition
       ( Block(..)
       , Inline(..)
       , Blocks
       , Inlines
       , Label(..)
       , Source(..)
       , Format(..)
       , QuoteType(..)
       , ListAttr(..)
       , ListStyle(..)
       , Key(..)
       , Attr(..)
       , nullAttr
       , (<>)
       , Listable(..)
       , trimInlines
       )

where
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, viewr, ViewR(..), (|>), ViewL(..), viewl)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import Data.List (intersperse)
import qualified Data.Foldable as F
import Data.String
import Data.Generics.Uniplate.Data

data Block = Para Inlines
           | Plain Inlines
           | Quote Blocks
           | Code Attr Text
           | List ListAttr [Blocks]
           | Header Int Inlines
           | RawBlock Format Text
           | HRule
           deriving (Show, Read, Data, Ord, Eq, Typeable)

data Inline = Txt Text
            | Sp
            | Emph Inlines
            | Strong Inlines
            | Image Label Source
            | Link Label Source
            | Verbatim Attr Text
            | Quoted QuoteType Inlines
            | LineBreak
            | RawInline Format Text
            | Note Key Blocks
            deriving (Show, Read, Data, Ord, Eq, Typeable)

data QuoteType = SingleQuoted | DoubleQuoted
               deriving (Show, Read, Data, Ord, Eq, Typeable)

newtype Label = Label Inlines
              deriving (Show, Read, Data, Ord, Eq, Typeable)

data Source = Source { location :: Text, title :: Text }
            | Ref { key :: Key, fallback :: Inlines }
            deriving (Show, Read, Data, Ord, Eq, Typeable)

newtype Format = Format Text
               deriving (Show, Read, Data, Typeable)

instance Eq Format where
  Format x == Format y = T.toUpper x == T.toUpper y

instance Ord Format where
  Format x `compare` Format y = T.toUpper x `compare` T.toUpper y

newtype Key = Key Inlines
              deriving (Show, Read, Data, Typeable)

instance Eq Key where
  Key x == Key y = transformBi T.toUpper x == transformBi T.toUpper y

instance Ord Key where
  Key x `compare` Key y = transformBi T.toUpper x `compare` transformBi T.toUpper y

data Attr = Attr { attrId      :: Text
                 , attrClasses :: [Text]
                 , attrKeyVals :: [(Text, Text)] }
                 deriving (Show, Read, Data, Ord, Eq, Typeable)

data ListAttr = ListAttr { listTight  :: Bool
                         , listStyle  :: ListStyle }
               deriving (Show, Read, Data, Ord, Eq, Typeable)

data ListStyle = Bullet | Ordered
               deriving (Show, Read, Data, Ord, Eq, Typeable)

nullAttr :: Attr
nullAttr = Attr "" [] []

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

newtype Inlines = Inlines { unInlines :: Seq Inline }
                deriving (Data, Ord, Eq, Typeable)

-- We show an Inlines just like [Inline].
instance Show Inlines where
  show = show . F.toList . unInlines

instance Read Inlines where
  readsPrec n = map (\(x,y) -> (Inlines . Seq.fromList $ x, y)) . readsPrec n

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
  fromString = Inlines . Seq.fromList . intersperse Sp . map Txt . T.words . T.pack

newtype Blocks = Blocks { unBlocks :: Seq Block }
                deriving (Data, Ord, Eq, Typeable, Monoid)

-- We show a Blocks just like [Block].
instance Show Blocks where
  show = show . F.toList . unBlocks

instance Read Blocks where
  readsPrec n = map (\(x,y) -> (Blocks . Seq.fromList $ x, y)) . readsPrec n

class Listable a b where
  toItems    :: a -> [b]
  fromItems  :: [b] -> a
  mapItems   :: (b -> a) -> a -> a
  single     :: b -> a
  foldItemsM :: Monad m => (a -> b -> m a) -> a -> a -> m a

instance Listable Inlines Inline where
  toItems        = F.toList . unInlines
  fromItems      = Inlines . Seq.fromList
  mapItems f     = F.foldMap f . unInlines
  single         = Inlines . Seq.singleton
  foldItemsM f x = F.foldlM f x . unInlines

instance Listable Blocks Block where
  toItems        = F.toList . unBlocks
  fromItems      = Blocks . Seq.fromList
  mapItems f     = F.foldMap f . unBlocks
  single         = Blocks . Seq.singleton
  foldItemsM f x = F.foldlM f x . unBlocks

-- | Trim leading and trailing Sp (spaces) from an Inlines.
trimInlines :: Inlines -> Inlines
trimInlines (Inlines ils) = Inlines $ Seq.dropWhileL (== Sp) $
                            Seq.dropWhileR (== Sp) $ ils

