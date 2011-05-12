{-# LANGUAGE DeriveDataTypeable #-}
import Text.Pandoc
import Text.Blaze.Renderer.Utf8
import Data.ByteString as B
import System.Environment
import Data.Text.Encoding (decodeUtf8)
import Data.Text as T
import System.Console.CmdArgs

main :: IO ()
main = do
  opts <- cmdArgs markdownOpts
  let tabstop = tab_stop opts
  let convert x = parseWith poptions pDoc (convertTabs tabstop $ decodeUtf8 x)
                  >>= renderHtmlToByteStringIO B.putStr . docToHtml poptions
  case files opts of
       [] -> B.getContents >>= convert
       fs -> mapM_ (\f -> B.readFile f >>= convert) fs

data Markdown = Markdown
    { tab_stop     :: Int
    , files       :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)

markdownOpts = Markdown
    { tab_stop    = def &= groupname "Options" &= opt (4 :: Int) &= help "Tab stop"
    , files       = def &= args &= typ "FILE.."
    } &=
    -- verbosity &=
    program "markdown" &=
    help "Convert markdown-formatted text to HTML" &=
    summary "pandoc v2, (c) John MacFarlane 2011" &=
    details []
