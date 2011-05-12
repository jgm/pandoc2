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
  verbosity' <- getVerbosity
  let poptions' = poptions { optLogLevel = case verbosity' of
                                                Quiet  -> ERROR
                                                Normal -> WARNING
                                                Loud   -> INFO }
  let convert x = parseWith poptions' pDoc
                  (convertTabs (tab_stop opts) $ decodeUtf8 x)
                  >>= renderHtmlToByteStringIO B.putStr . docToHtml poptions
  case files opts of
       [] -> B.getContents >>= convert
       fs -> mapM_ (\f -> B.readFile f >>= convert) fs

data Pandoc2 = Pandoc2
    { tab_stop    :: Int
    , files       :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)

opts = Pandoc2
    { tab_stop    = 4 &= groupname "Options" &= help "Tab stop"
    , files       = def &= args &= typ "FILE.."
    } &=
    program "pandoc2" &=
    verbosity &=
    help "Convert between text formats" &=
    summary "pandoc2 v2, (c) John MacFarlane 2011" &=
    details []
