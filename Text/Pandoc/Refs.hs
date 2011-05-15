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
resolveRefs bs = F.foldlM handleRefB mempty $ unBlocks bs

handleRefB :: PMonad m => Blocks -> Block -> P m Blocks
handleRefB acc x = do
  let goI = F.foldlM handleRefI mempty . unInlines
  let goB = F.foldlM handleRefB mempty . unBlocks
  res <- case x of
            (Para ils)       -> para <$> goI ils
            (Plain ils)      -> block . Plain <$> goI ils
            (Quote bs)       -> quote <$> goB bs
            (List attr its)  -> block . List attr <$> mapM goB its
            (Header lev ils) -> header lev <$> goI ils
            _                -> return $ block x
  return $ acc <> res

handleRefI :: PMonad m => Inlines -> Inline -> P m Inlines
handleRefI acc x = do
  st <- getState
  let goI = F.foldlM handleRefI mempty . unInlines
  let goB = F.foldlM handleRefB mempty . unBlocks
  res <- case x of
            (Emph ils)       -> inline . Emph <$> goI ils
            (Strong ils)     -> inline . Strong <$> goI ils
            (Quoted qt ils)  -> inline . Quoted qt <$> goI ils
            (Link (Label lab) Ref{ key = k, fallback = ils }) ->
              case M.lookup k (sReferences st) of
                   Just s  -> do lab' <- goI lab
                                 return $ inline $ Link (Label lab') s
                   Nothing -> goI ils
            (Image (Label lab) Ref{ key = k, fallback = ils }) ->
              case M.lookup k (sReferences st) of
                   Just s  -> do lab' <- goI lab
                                 return $ inline $ Image (Label lab') s
                   Nothing -> goI ils
            (Note (Key k) bs) | k == mempty ->
                   inline . Note (Key mempty) <$> goB bs
            (Note (Key k) _) ->
              case M.lookup (Key k) (sNotes st) of
                   Just bs -> inline . Note (Key mempty) <$> goB bs
                   Nothing -> goI k
            _                -> return $ inline x
  return $ acc <> res
