{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MultiParamTypeClasses #-}

module Builder where

import Types
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

