import Pandoc
import Data.Text.IO as T
import Text.Blaze.Renderer.Utf8
import Data.ByteString as B

main = do
  T.getContents >>= parseWith pDoc >>= renderHtmlToByteStringIO B.putStrLn . blocksToHtml

