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

inline :: Inline -> Inlines
inline = Inlines . singleton

-- | Convert a 'Text' to 'Inlines', treating interword spaces as 'Sp's.
-- If you want a 'Str' with literal spaces, use 'literal'.
txt :: T.Text -> Inlines
txt = Inlines . fromList . intersperse Sp . map Txt . T.words

literal :: Text -> Inlines
literal = inline . Txt

emph :: Inlines -> Inlines
emph = inline . Emph

link :: Inlines -> Text -> Text -> Inlines
link lab tit src = inline $ Link (Label lab) (Title tit) (Source src)

sp :: Inlines
sp = inline Sp

