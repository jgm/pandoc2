{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Text.Pandoc.Builder
where
import Text.Pandoc.Definition
import Data.Sequence hiding (null)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Generics.Uniplate.Data

-- Pandoc builder DSL

inline :: Inline -> Inlines
inline = Inlines . singleton

txt :: Text -> Inlines
txt = inline . Txt

(<+>) :: Inlines -> Inlines -> Inlines
x <+> y = x <> inline Sp <> y

emph :: Inlines -> Inlines
emph = inline . Emph

strong :: Inlines -> Inlines
strong = inline . Strong

link :: Inlines -> Source -> Inlines
link lab src = inline $ Link (Label lab) src

image :: Inlines -> Source -> Inlines
image lab src = inline $ Image (Label lab) src

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

quote :: Blocks -> Blocks
quote = block . Quote

codeAttr :: Attr -> Text -> Blocks
codeAttr attr = block . Code attr

code :: Text -> Blocks
code = codeAttr nullAttr

orderedListTight :: [Blocks] -> Blocks
orderedListTight =
  block . List ListAttr{ listTight = True, listStyle = Ordered }

orderedListLoose :: [Blocks] -> Blocks
orderedListLoose =
  block . List ListAttr{ listTight = False, listStyle = Ordered }

bulletListTight :: [Blocks] -> Blocks
bulletListTight =
  block . List ListAttr{ listTight = True, listStyle = Bullet }

bulletListLoose :: [Blocks] -> Blocks
bulletListLoose =
  block . List ListAttr{ listTight = False, listStyle = Bullet }

header :: Int -> Inlines -> Blocks
header n = block . Header n

rawBlock :: Format -> Text -> Blocks
rawBlock f = block . RawBlock f

hrule :: Blocks
hrule = block HRule

-----------------

textify :: Inlines -> Text
textify = T.concat . map extractText . universeBi
  where extractText :: Inline -> Text
        extractText (Txt t)   = t
        extractText Sp        = " "
        extractText LineBreak = " "
        extractText _         = ""
