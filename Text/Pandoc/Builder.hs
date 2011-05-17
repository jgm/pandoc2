{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Text.Pandoc.Builder
where
import Text.Pandoc.Definition
import qualified Data.Text as T
import Data.Text (Text)

-- Pandoc builder DSL

txt :: Text -> Inlines
txt = single . Txt

ch :: Char -> Inlines
ch = single . Txt . T.singleton

(<+>) :: Inlines -> Inlines -> Inlines
x <+> y = x <> single Sp <> y

emph :: Inlines -> Inlines
emph = single . Emph

strong :: Inlines -> Inlines
strong = single . Strong

link :: Inlines -> Source -> Inlines
link lab src = single $ Link (Label lab) src

image :: Inlines -> Source -> Inlines
image lab src = single $ Image (Label lab) src

verbatim :: Text -> Inlines
verbatim = verbatimAttr nullAttr

verbatimAttr :: Attr -> Text -> Inlines
verbatimAttr attr = single . Verbatim attr

singleQuoted :: Inlines -> Inlines
singleQuoted = single . Quoted SingleQuoted

doubleQuoted :: Inlines -> Inlines
doubleQuoted = single . Quoted DoubleQuoted

lineBreak :: Inlines
lineBreak = single LineBreak

math :: MathType -> Text -> Inlines
math t = single . Math t

rawInline :: Format -> Text -> Inlines
rawInline f = single . RawInline f

note :: Blocks -> Inlines
note = single . Note

para :: Inlines -> Blocks
para = single . Para

plain :: Inlines -> Blocks
plain = single . Plain

quote :: Blocks -> Blocks
quote = single . Quote

codeAttr :: Attr -> Text -> Blocks
codeAttr attr = single . Code attr

code :: Text -> Blocks
code = codeAttr nullAttr

orderedListTight :: [Blocks] -> Blocks
orderedListTight =
  single . List ListAttr{ listTight = True, listStyle = Ordered }

orderedListLoose :: [Blocks] -> Blocks
orderedListLoose =
  single . List ListAttr{ listTight = False, listStyle = Ordered }

bulletListTight :: [Blocks] -> Blocks
bulletListTight =
  single . List ListAttr{ listTight = True, listStyle = Bullet }

bulletListLoose :: [Blocks] -> Blocks
bulletListLoose =
  single . List ListAttr{ listTight = False, listStyle = Bullet }

header :: Int -> Inlines -> Blocks
header n = single . Header n

rawBlock :: Format -> Text -> Blocks
rawBlock f = single . RawBlock f

hrule :: Blocks
hrule = single HRule

