import Pandoc
import Data.Text.IO as T
import Text.Blaze.Renderer.Utf8
import Data.ByteString as B
import System.Environment
import Data.Text (Text)
import Data.Text as T

main = do
  args <- getArgs
  let convert x = parseWith pDoc x
                  >>= renderHtmlToByteStringIO B.putStr . blocksToHtml
  case args of
       [] -> T.getContents >>= convert . convertTabs
       _  -> mapM_ (\f -> T.readFile f >>= convert . convertTabs) args

convertTabs :: Text -> Text
convertTabs = T.unlines . Prelude.map convertTabL . T.lines
  where convertTabL l =
          case T.break (=='\t') l of
                (_,x) | T.null x -> l
                (x,y) -> x <> ss <> convertTabL (T.tail y)
                          where ss = T.replicate (4 - (n `mod` 4)) s
                                s  = T.pack " "
                                n  = T.length x
