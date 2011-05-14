module Text.Pandoc.Refs (resolveRefs)
where
import Text.Pandoc.Definition
import Text.Pandoc.Parsing
import Text.Pandoc.Builder
import qualified Data.Map as M
import qualified Data.Foldable as F
import Text.Parsec (getState)
import Control.Applicative ((<$>))
import Data.Monoid

-- Reference resolution

resolveRefs :: PMonad m => Blocks -> P m Blocks
resolveRefs bs = do
  st <- getState
  return $ F.foldMap (handleRefB st) $ unBlocks bs

handleRefB :: PMonad m => PState m -> Block -> Blocks
handleRefB st x =
  let goI = F.foldMap (handleRefI st) . unInlines
      goB = F.foldMap (handleRefB st) . unBlocks
  in case x of
      (Para ils)       -> para $ goI ils
      (Plain ils)      -> block $ Plain $ goI ils
      (Quote bs)       -> quote $ goB bs
      (List attr its)  -> block $ List attr $ map goB its
      (Header lev ils) -> header lev $ goI ils
      _                -> block x

handleRefI :: PMonad m => PState m -> Inline -> Inlines
handleRefI st x =
  let goI = F.foldMap (handleRefI st) . unInlines
      goB = F.foldMap (handleRefB st) . unBlocks
  in case x of
      (Emph ils)       -> inline $ Emph $ goI ils
      (Strong ils)     -> inline $ Strong $ goI ils
      (Quoted qt ils)  -> inline $ Quoted qt $ goI ils
      (Link (Label lab) Ref{ key = k, fallback = ils }) ->
        case M.lookup k (sReferences st) of
             Just s  -> inline $ Link (Label $ goI lab) s
             Nothing -> goI ils
      (Image (Label lab) Ref{ key = k, fallback = ils }) ->
        case M.lookup k (sReferences st) of
             Just s  -> inline $ Image (Label $ goI lab) s
             Nothing -> goI ils
      (Note (Key k) bs) | k == mempty -> inline $ Note (Key mempty) $ goB bs
      (Note (Key k) _) ->
        case M.lookup (Key k) (sNotes st) of
             Just bs -> inline $ Note (Key mempty) $ goB bs
             Nothing -> goI k
      _                -> inline x

