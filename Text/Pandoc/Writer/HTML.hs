{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
    TypeSynonymInstances #-}
module Text.Pandoc.Writer.HTML (docToHtml) where
import Text.Pandoc.Definition
import Text.Pandoc.Builder (textify)
import Text.Pandoc.Shared (POptions)
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Generics.Uniplate.Operations (transformBi)
import Data.List (intersperse)
import Control.Monad.State
import Control.Applicative

data WriterState = WriterState { wOptions :: POptions
                               , wNotes   :: [Html] }

type W = State WriterState Html

instance Monoid W where
  mempty = return mempty
  mappend = liftM2 mappend

nl :: W
nl = return $ preEscapedText "\n"

docToHtml :: POptions -> Blocks -> Html
docToHtml opts bs =
  evalState goHtml WriterState{ wOptions = opts, wNotes   = [] }
    where goHtml = do body <- blocksToHtml bs
                      notes <- reverse . wNotes <$> get
                      spacer <- nl
                      let fnblock = H.ol ! A.id "footnotes"
                                        $  spacer
                                        <> mconcat (intersperse spacer notes)
                                        <> spacer
                      return $ body <> fnblock

blocksToHtml :: Blocks -> W
blocksToHtml = F.foldMap (\b -> blockToHtml b <> nl) . unBlocks

blockToHtml :: Block -> W
blockToHtml (Para ils) = H.p <$> inlinesToHtml ils
blockToHtml (Plain ils) = inlinesToHtml ils
blockToHtml (Quote bs) = H.blockquote <$> nl <> blocksToHtml bs
blockToHtml (List attr bs) =
  let paraToPlain (Para xs) = Plain xs
      paraToPlain x         = x
      bs' = if listTight attr
               then transformBi paraToPlain bs
               else bs
      items = F.foldMap (\b -> (H.li <$> blocksToHtml b) <> nl) bs'
  in  case listStyle attr of
           Bullet    -> H.ul <$> nl <> items
           Ordered   -> H.ol <$> nl <> items
blockToHtml (Code _attr t) = return $ H.pre $ H.code $ toHtml t
blockToHtml (RawBlock (Format "html") t) = return $ preEscapedText t
blockToHtml (RawBlock _ _) = return mempty
blockToHtml (Header lev ils) = h <$> inlinesToHtml ils
  where h = case lev of
             1  -> H.h1
             2  -> H.h2
             3  -> H.h3
             4  -> H.h4
             5  -> H.h5
             _  -> H.p
blockToHtml HRule = return H.hr

inlinesToHtml :: Inlines -> W
inlinesToHtml = F.foldMap inlineToHtml . unInlines

inlineToHtml :: Inline -> W
inlineToHtml (Txt x) = return $ toHtml x
inlineToHtml Sp      = return $ toHtml (" " :: L.Text)
inlineToHtml (Emph ils) = H.em <$> inlinesToHtml ils
inlineToHtml (Strong ils) = H.strong <$> inlinesToHtml ils
inlineToHtml (Link _ Ref{}) =
  error "Encountered Ref link!  Please report bug to pandoc maintainers."
inlineToHtml (Link (Label lab) src@Source{}) = do
  let tit = title src
  x <- (H.a ! A.href (toValue $ location src)) <$> inlinesToHtml lab
  return $ if T.null tit then x else x ! A.title (toValue $ title src)
inlineToHtml (Image _ Ref{}) =
  error "Encountered Ref image!  Please report bug to pandoc maintainers."
inlineToHtml (Image (Label lab) src@Source{}) = return $
  H.img ! A.src (toValue $ location src) ! A.title (toValue $ title src)
        ! A.alt (toValue $ textify lab)
inlineToHtml LineBreak = return $ H.br
inlineToHtml (RawInline (Format "html") t) = return $ preEscapedText t
inlineToHtml (RawInline _ _) = return $ mempty
inlineToHtml (Verbatim _attr t) = return $ H.code $ toHtml t
inlineToHtml (Quoted SingleQuoted ils) = do
  xs <- inlinesToHtml ils
  return $ "\8216" <> xs <> "\8217"
inlineToHtml (Quoted DoubleQuoted ils) = do
  xs <- inlinesToHtml ils
  return $ "\8220" <> xs <> "\8221"
inlineToHtml (Note bs) = do
  notes <- wNotes <$> get
  let nextnum = show $ length notes + 1
  let refid = "fnref" ++ nextnum
  let noteid = "fn" ++ nextnum
  let marker = H.sup $ toHtml nextnum
  contents <- blocksToHtml bs
  let fn = H.li ! A.id (toValue noteid)
                ! A.href (toValue $ '#':refid)
                ! A.class_ "footnote"
                $ contents
  modify $ \st -> st{ wNotes = fn : notes }
  return $ marker ! A.id (toValue refid)
                  ! A.href (toValue $ '#':noteid)
