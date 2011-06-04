{-# LANGUAGE DeriveDataTypeable #-}
import Text.Pandoc2
import Text.Blaze.Renderer.Utf8
import qualified Data.ByteString as B
import System.Environment
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Data.Char (toLower)
import Data.List (intercalate)
import System.Console.CmdArgs

main :: IO ()
main = do
  opts <- cmdArgs opts
  verbosity' <- getVerbosity
  let poptions' = poptions { optLogLevel = case verbosity' of
                                                Quiet  -> ERROR
                                                Normal -> WARNING
                                                Loud   -> INFO
                           , optTabStop = tab_stop opts
                           , optExtensions =
                               if strict opts
                                  then noExtensions
                                  else setExtensions $ extension opts
                           , optSmart = smart opts
                           , optMathMethod = math_method opts
                           }
  let convert = markdownDoc poptions' . decodeUtf8
  let render = case map toLower (to opts) of
                    "html"   -> renderHtmlToByteStringIO B.putStr .
                                 docToHtml poptions'
                    "native" -> print
                    _        -> error $ "Unknown writer " ++ show (to opts)
  case files opts of
       [] -> B.getContents >>= convert >>= render
       fs -> mapM_ (\f -> B.readFile f >>= convert >>= render) fs

data Pandoc2 = Pandoc2
    { tab_stop    :: Int
    , files       :: [FilePath]
    , strict      :: Bool
    , extension   :: [PExtension]
    , smart       :: Bool
    , math_method :: HTMLMathMethod
    , to          :: String
    , from        :: String
    }
    deriving (Data,Typeable,Show,Eq)

{-
extensionsHelp :: String
extensionsHelp =
  "Syntax extensions: " ++ intercalate ", " (map show (enumFrom Footnotes))
-}

opts = Pandoc2
    { from        = "markdown" &= typ "FORMAT" &= help "Source format"
    , to          = "html" &= typ "FORMAT" &= help "Target format"
    , strict      = def &= help "Disable pandoc's markdown extensions"
    , extension   = enumFrom Footnotes &=
                      typ "EXTENSION" &= help "Selectively enable syntax extension"
    , smart       = def &= help "Enable smart typography"
    , math_method = PlainMath &= typ "MATHMETHOD" &= help "How to display math in HTML"
    , tab_stop    = 4 &= groupname "Options" &= explicit &= name "tab-stop"
                      &= help "Tab stop"
    , files       = def &= args &= typ "FILE.."
    } &=
    program "pandoc2" &=
    verbosity &=
    help "Convert between text formats" &=
    summary "pandoc2 v2, (c) John MacFarlane 2011" &=
    details [ "Input formats:  markdown"
            , "Output formats: html, native"
            ]
