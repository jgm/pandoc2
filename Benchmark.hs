import Text.Pandoc2
import Criterion.Main
import Data.List (isSuffixOf)
import Text.JSON.Generic
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString as B

main = do
  inp <- B.readFile "README.markdown"
  let poptions' = poptions
  let convert :: B.ByteString -> Maybe Blocks
      convert = markdownDoc poptions' . decodeUtf8
  defaultMain [ bench ("markdown reader") $ whnf convert inp ]

