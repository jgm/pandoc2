module Text.Pandoc.Refs (resolveRefs)
where
import Text.Pandoc.Definition
import Text.Pandoc.Parsing
import Text.Pandoc.Builder
import qualified Data.Map as M
import Text.Parsec (getState)
import Control.Applicative ((<$>))
import Data.Monoid

-- Reference resolution

resolveRefs :: PMonad m => Blocks -> P m Blocks
resolveRefs = foldItemsM handleRefB mempty

handleRefB :: PMonad m => Blocks -> Block -> P m Blocks
handleRefB acc x = do
  let goI = foldItemsM handleRefI mempty
  let goB = foldItemsM handleRefB mempty
  res <- case x of
            (Para ils)       -> para <$> goI ils
            (Plain ils)      -> plain <$> goI ils
            (Quote bs)       -> quote <$> goB bs
            (List attr its)  -> single . List attr <$> mapM goB its
            (Header lev ils) -> header lev <$> goI ils
            _                -> return $ single x
  return $ acc <> res

handleRefI :: PMonad m => Inlines -> Inline -> P m Inlines
handleRefI acc x = do
  st <- getState
  let goI = foldItemsM  handleRefI mempty
  let goB = foldItemsM handleRefB mempty
  res <- case x of
            (Emph ils)       -> single . Emph <$> goI ils
            (Strong ils)     -> single . Strong <$> goI ils
            (Quoted qt ils)  -> single . Quoted qt <$> goI ils
            (Link (Label lab) Ref{ key = k, fallback = ils }) ->
              case M.lookup k (sReferences st) of
                   Just s  -> do lab' <- goI lab
                                 return $ single $ Link (Label lab') s
                   Nothing -> goI ils
            (Image (Label lab) Ref{ key = k, fallback = ils }) ->
              case M.lookup k (sReferences st) of
                   Just s  -> do lab' <- goI lab
                                 return $ single $ Image (Label lab') s
                   Nothing -> goI ils
            (Note (Key k) bs) | k == mempty ->
                   single . Note (Key mempty) <$> goB bs
            (Note (Key k) _) ->
              case M.lookup (Key k) (sNotes st) of
                   Just bs -> single . Note (Key mempty) <$> goB bs
                   Nothing -> goI k
            _                -> return $ single x
  return $ acc <> res
