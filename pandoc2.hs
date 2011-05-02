{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Data.Sequence hiding (null)
import Data.Monoid
import Data.Generics.Uniplate.Direct
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import Data.List (intersperse)
import Data.Foldable (toList, foldMap)
import qualified Text.ParserCombinators.ReadP as R
import Data.Generics

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
                          (Txt t1, Txt t2)   -> xs' |> Txt (t1 `mappend` t2)
                          (Emph i1, Emph i2) -> xs' |> Emph (i1 `mappend` i2)
                          _                  -> xs' |> x |> y

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

newtype Blocks = Blocks { unBlocks :: Seq Block }
                deriving (Data, Ord, Eq, Typeable)

-- We show a Blocks just like [Block].
instance Show Blocks where
  show = show . toList . unBlocks

instance Read Blocks where
  readsPrec n = map (\(x,y) -> (Blocks . fromList $ x, y)) . readsPrec n

data Inline = Txt Text
            | Sp
            | Emph Inlines
            deriving (Show, Read, Data, Ord, Eq, Typeable)

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

-- just testing generics:

bottomUp f = everywhere (mkT f)

gentest :: Inline -> Inline
gentest (Txt t) = Txt $ T.toUpper t
gentest x = x

--


