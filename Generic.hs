{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, MultiParamTypeClasses #-}
module Generic
where
import Types
import Data.Sequence hiding (null)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import Data.List (intersperse)
import Data.Foldable (toList)
import Data.Generics
import Text.Parsec
import Control.Monad.Identity (Identity)

-- just testing generics:

bottomUp f = everywhere (mkT f)

gentest :: Inline -> Inline
gentest (Txt t) = Txt $ T.toUpper t
gentest x = x

gentest2 :: Title -> Title
gentest2 (Title x) = (Title "")


-- awkward: still hard to remove an item from a list and replace it with
-- several
gentest3 :: Seq Inline -> Seq Inline
gentest3 x | Data.Sequence.take 1 x == fromList [Txt "hi"] = Txt "H" <| Sp <| Txt "I" <| Data.Sequence.drop 1 x
gentest3 x = x

