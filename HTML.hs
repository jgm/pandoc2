{-# LANGUAGE OverloadedStrings #-}
module HTML where
import Definition
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Foldable as F
import Data.Text.Lazy (Text)

nl :: Html
nl = preEscapedText "\n"

blocksToHtml :: Blocks -> Html
blocksToHtml = F.mapM_ (\b -> blockToHtml b >> nl) . unBlocks

blockToHtml :: Block -> Html
blockToHtml (Para ils) = H.p $ inlinesToHtml ils
blockToHtml (Quote bs) = H.blockquote $ nl >> blocksToHtml bs

inlinesToHtml :: Inlines -> Html
inlinesToHtml = F.mapM_ inlineToHtml . unInlines

inlineToHtml :: Inline -> Html
inlineToHtml (Txt x) = toHtml x
inlineToHtml Sp      = toHtml (" " :: Text)
inlineToHtml (Link (Label lab) src) =
  H.a ! A.href (toValue $ location src) ! A.title (toValue $ title src)
  $ inlinesToHtml lab

