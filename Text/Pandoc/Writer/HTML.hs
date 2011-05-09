{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Writer.HTML where
import Text.Pandoc.Definition
import Text.Pandoc.Builder (textify)
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Foldable as F
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Generics.Uniplate.Operations (transformBi)

nl :: Html
nl = preEscapedText "\n"

blocksToHtml :: Blocks -> Html
blocksToHtml = F.foldMap (\b -> blockToHtml b <> nl) . unBlocks

blockToHtml :: Block -> Html
blockToHtml (Para ils) = H.p $ inlinesToHtml ils
blockToHtml (Plain ils) = inlinesToHtml ils
blockToHtml (Quote bs) = H.blockquote $ nl <> blocksToHtml bs
blockToHtml (List attr bs) =
  let paraToPlain (Para xs) = Plain xs
      paraToPlain x         = x
      bs' = if listTight attr
               then transformBi paraToPlain bs
               else bs
      items = F.foldMap (\b -> H.li (blocksToHtml b) <> nl) bs'
  in  case listStyle attr of
           Bullet    -> H.ul $ nl <> items
           Ordered   -> H.ol $ nl <> items
blockToHtml (Code _attr t) = H.pre $ H.code $ toHtml t
blockToHtml (RawBlock (Format "html") t) = preEscapedText t
blockToHtml (RawBlock _ _) = mempty
blockToHtml (Header lev ils) = h $ inlinesToHtml ils
  where h = case lev of
             1  -> H.h1
             2  -> H.h2
             3  -> H.h3
             4  -> H.h4
             5  -> H.h5
             _  -> H.p
blockToHtml HRule = H.hr

inlinesToHtml :: Inlines -> Html
inlinesToHtml = F.foldMap inlineToHtml . unInlines

inlineToHtml :: Inline -> Html
inlineToHtml (Txt x) = toHtml x
inlineToHtml Sp      = toHtml (" " :: L.Text)
inlineToHtml (Emph ils) = H.em $ inlinesToHtml ils
inlineToHtml (Strong ils) = H.strong $ inlinesToHtml ils
inlineToHtml (Link _ Ref{}) = error "Encountered Ref link!"
inlineToHtml (Link (Label lab) src@Source{}) =
  let tit = title src
      x = H.a ! A.href (toValue $ location src) $ inlinesToHtml lab
  in  if T.null tit then x else x ! A.title (toValue $ title src)
inlineToHtml (Image _ Ref{}) = error "Encountered Ref image!"
inlineToHtml (Image (Label lab) src@Source{}) =
  H.img ! A.src (toValue $ location src) ! A.title (toValue $ title src)
        ! A.alt (toValue $ textify lab)
inlineToHtml LineBreak = H.br
inlineToHtml (RawInline (Format "html") t) = preEscapedText t
inlineToHtml (RawInline _ _) = mempty
inlineToHtml (Verbatim _attr t) = H.code $ toHtml t

