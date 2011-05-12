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
  opts <- cmdArgs opts
  let convert x = parseWith poptions pDoc
                  (convertTabs (tab_stop opts) $ decodeUtf8 x)
                  >>= renderHtmlToByteStringIO B.putStr . docToHtml poptions
  case files opts of
       [] -> B.getContents >>= convert
       fs -> mapM_ (\f -> B.readFile f >>= convert) fs

data Pandoc2 = Pandoc2
    { tab_stop    :: Int
    , log_level   :: LogLevel
    , files       :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)

opts = Pandoc2
    { tab_stop    = 4 &= help "Tab stop"
    , log_level   = WARNING &= help "Severity of messages to log"
    , files       = def &= args &= typ "FILE.."
    } &=
    -- verbosity &=
    program "pandoc2" &=
    help "Convert between text formats" &=
    summary "pandoc2 v2, (c) John MacFarlane 2011" &=
    details []
