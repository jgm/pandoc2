{-# LANGUAGE OverloadedStrings #-}
module HTML where
import Definition
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Foldable as F
import Data.Text.Lazy (Text)
import Data.Generics.Uniplate.Operations (transformBi)

nl :: Html
nl = preEscapedText "\n"

blocksToHtml :: Blocks -> Html
blocksToHtml = F.mapM_ (\b -> blockToHtml b >> nl) . unBlocks

blockToHtml :: Block -> Html
blockToHtml (Para ils) = H.p $ inlinesToHtml ils
blockToHtml (Plain ils) = inlinesToHtml ils
blockToHtml (Quote bs) = H.blockquote $ nl >> blocksToHtml bs
blockToHtml (List attr bs) = do
  let paraToPlain (Para xs) = Plain xs
      paraToPlain x         = x
  let bs' = if listTight attr
               then transformBi paraToPlain bs
               else bs
  let items = F.mapM_ (\b -> H.li (blocksToHtml b) >> nl) bs'
  case listStyle attr of
       Bullet    -> H.ul $ nl >> items
       Ordered   -> H.ol $ nl >> items
blockToHtml (Code _attr t) = H.pre $ H.code $ toHtml t

inlinesToHtml :: Inlines -> Html
inlinesToHtml = F.mapM_ inlineToHtml . unInlines

inlineToHtml :: Inline -> Html
inlineToHtml (Txt x) = toHtml x
inlineToHtml Sp      = toHtml (" " :: Text)
inlineToHtml (Link (Label lab) src) =
  H.a ! A.href (toValue $ location src) ! A.title (toValue $ title src)
  $ inlinesToHtml lab

