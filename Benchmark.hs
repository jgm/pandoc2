import Text.Pandoc
import Criterion.Main
import Data.List (isSuffixOf)
import Data.Text as T
import Data.Text.IO as T
import Data.Sequence as Seq
import Data.Maybe (fromJust)

readerBench :: Text
            -> (String, Text -> Maybe Blocks)
            -> Benchmark
readerBench doc (name, reader) =
  -- we compute the length to force full evaluation
  bench (name ++ " reader") $ whnf (Seq.length . unBlocks . fromJust . reader) doc

main = do
  doc <- T.readFile "Tests/Tests_Markdown_1.0.3/Markdown Documentation - Syntax.text"
  let readerBs = [readerBench doc ("pandoc2 markdown", parseWith poptions pDoc)]
  defaultMain readerBs
