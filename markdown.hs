import Pandoc
import Data.Text.IO as T
import Text.Blaze.Renderer.Utf8
import Data.ByteString as B
import System.Environment

main = do
  args <- getArgs
  let convert x = parseWith pDoc x
                  >>= renderHtmlToByteStringIO B.putStr . blocksToHtml
  case args of
       [] -> T.getContents >>= convert
       _  -> mapM_ (\f -> T.readFile f >>= convert) args
