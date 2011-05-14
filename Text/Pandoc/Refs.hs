module Text.Pandoc.Refs (resolveRefs)
where
import Text.Pandoc.Definition
import Text.Pandoc.Parsing
import Text.Pandoc.Builder
import qualified Data.Map as M
import qualified Data.Foldable as F
import Text.Parsec (getState)
import Control.Applicative ((<$>))

-- Reference resolution

resolveRefs :: PMonad m => Blocks -> P m Blocks
resolveRefs bs = do
  refs <- sReferences <$> getState
  return $ handleRefs refs bs

handleRefs :: M.Map Key Source -> Blocks -> Blocks
handleRefs refs = F.foldMap (handleRefB refs) . unBlocks

handleRefB :: M.Map Key Source -> Block -> Blocks
handleRefB refs x =
  let goI = F.foldMap (handleRefI refs) . unInlines
      goB = F.foldMap (handleRefB refs) . unBlocks
  in case x of
      (Para ils)       -> para $ goI ils
      (Plain ils)      -> block $ Plain $ goI ils
      (Quote bs)       -> quote $ goB bs
      (List attr its)  -> block $ List attr $ map goB its
      (Header lev ils) -> header lev $ goI ils
      _                -> block x

handleRefI :: M.Map Key Source -> Inline -> Inlines
handleRefI refs x =
  let goI = F.foldMap (handleRefI refs) . unInlines
      goB = F.foldMap (handleRefB refs) . unBlocks
  in case x of
      (Emph ils)       -> inline $ Emph $ goI ils
      (Strong ils)     -> inline $ Strong $ goI ils
      (Quoted qt ils)  -> inline $ Quoted qt $ goI ils
      (Link (Label lab) Ref{ key = k, fallback = ils }) ->
        case M.lookup k refs of
             Just s  -> inline $ Link (Label $ goI lab) s
             Nothing -> goI ils
      (Image (Label lab) Ref{ key = k, fallback = ils }) ->
        case M.lookup k refs of
             Just s  -> inline $ Image (Label $ goI lab) s
             Nothing -> goI ils
      (Note bs)        -> inline $ Note $ goB bs
      _                -> inline x

