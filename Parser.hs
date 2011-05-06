{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses,  GeneralizedNewtypeDeriving #-}
module Parser
where
import Definition
import Data.Sequence as Seq
import Data.Monoid
import qualified Data.Map as M
import Data.Traversable
import qualified Data.Text as T
import Data.Text (Text)
import Data.Data
import qualified Data.ByteString.Char8 as B8
import Data.List (intersperse)
import qualified Data.Foldable as F
import Data.Generics
import Text.Parsec
import Control.Monad.Identity (Identity)
import Control.Monad
import Control.Arrow ((***))
import System.FilePath
import System.IO (stderr)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import Control.Applicative ((<$>), (<$), (*>), (<*))
import Data.Generics.Uniplate.Operations (transformBi)
import Network.URI ( escapeURIString, isAllowedInURI )

{- TODO:

x references (populate sReferences in state)
x emph
x strong
x escapes
x special chars
x block parser for random whitespace eg. at beg and end, returns mempty?
x proper escaping for URLs
x setext headers
x atx headers
x HTML writer using blaze
_ hrule
_ autolinks
_ explicit links
_ image (ref & explicit)
x verbatim
_ raw HTML inline
_ raw HTML blocks
x executable
_ tests
_ benchmarks

-}

data PState = PState {
                  sGetFile    :: FilePath -> P Text
                , sMessages   :: Seq Text
                , sLogLevel   :: LogLevel
                , sEndline    :: Seq (P ())
                , sBlockSep   :: Seq (P ())
                , sReferences :: M.Map Key Source
                }

pstate :: PState
pstate = PState { sGetFile  = undefined
                , sMessages = Seq.empty
                , sLogLevel = WARNING
                , sEndline  = Seq.empty
                , sBlockSep = Seq.empty
                , sReferences = M.empty
                }

type P a = ParsecT Text PState IO a

instance Stream Text IO Char where
  uncons = return . T.uncons

data LogLevel = DEBUG | INFO | WARNING | ERROR
              deriving (Ord, Eq, Show, Read)

pushEndline :: P () -> P ()
pushEndline p = modifyState $ \st -> st{ sEndline = sEndline st |> p }

popEndline :: P ()
popEndline = do
  st <- getState
  case viewr (sEndline st) of
        EmptyR  -> logM ERROR "Tried to pop empty pEndline stack"
        ps :> _ -> setState st{ sEndline = ps }

withEndline :: P a -> P b -> P b
withEndline sep p = pushEndline (sep *> return ()) *> p <* popEndline

pushBlockSep :: P () -> P ()
pushBlockSep p = modifyState $ \st -> st{ sBlockSep = sBlockSep st |> p }

popBlockSep :: P ()
popBlockSep = do
  st <- getState
  case viewr (sBlockSep st) of
        EmptyR  -> logM ERROR "Tried to pop empty pBlockSep stack"
        ps :> _ -> setState st{ sBlockSep = ps }

withBlockSep :: P a -> P b -> P b
withBlockSep sep p = pushBlockSep (sep *> return ()) *> p <* popBlockSep

pBlockSep :: P ()
pBlockSep = try (getState >>= sequenceA . sBlockSep) >> return ()

pNewlines :: P Int
pNewlines = Prelude.length <$> many1 pNewline

pNewline :: P Int
pNewline = try $ spnl *> pBlockSep *> return 1

pEndline :: P Inlines
pEndline = try $
  newline *> (getState >>= sequenceA . sEndline) *> skipMany spaceChar *>
  lookAhead nonnl *> return sp

pVerbatim :: P Inlines
pVerbatim = try $ do
  delim <- many1 (char '`')
  sps
  verbatim . T.pack
     <$> many1Till (nonnl <|> (' ' <$ (pNewline *> notFollowedBy spnl)))
            (try $ sps *> string delim *> notFollowedBy (char '`'))

nonnl :: P Char
nonnl = satisfy (/= '\n')

sps :: P ()
sps = skipMany spaceChar

spnl :: P ()
spnl = try $ sps <* newline

spOptNl :: P ()
spOptNl = try $ sps <* optional (newline <* sps)

spaceChar :: P Char
spaceChar = satisfy (\c -> c == ' ' || c == '\t')

showText :: Show a => a -> Text
showText = T.pack . show

logM :: LogLevel -> Text -> P ()
logM level msg = do
  logLevel <- fmap sLogLevel getState
  pos <- getPosition
  let msg' = showText level <> " (line " <>
             showText (sourceLine pos) <> " col " <>
             showText (sourceColumn pos) <> "): " <> msg
  if level >= logLevel
     then modifyState $ \st ->
              st { sMessages = sMessages st |> msg' }
     else return ()

parseWith :: P a -> Text -> IO a
parseWith p t = do
  let p' = do x <- p
              msgs <- fmap sMessages getState
              return (x, msgs)
  res <- runParserT p' pstate "input" t
  case res of
       Left err -> error $ show err
       Right (x, msgs) -> do
                   mapM_ (T.hPutStrLn stderr) $ F.toList msgs
                   return x

pInline :: P Inlines
pInline = choice [ pSp, pTxt, pEndline, pFours, pStrong, pEmph, pVerbatim,
                   pLink, pEscaped, pSymbol ]

toInlines :: [Inlines] -> Inlines
toInlines = trimInlines . mconcat

pInlines :: P Inlines
pInlines = toInlines <$> many1 pInline

pEscaped :: P Inlines
pEscaped = txt . T.singleton <$> (char '\\' *> oneOf "\\`*_{}[]()>#+-.!~")

pSymbol :: P Inlines
pSymbol = txt . T.singleton <$> nonnl

pSp :: P Inlines
pSp = spaceChar *> (  many1 spaceChar *> ((lineBreak <$ pEndline) <|> return sp)
                  <|> return sp)

pLink :: P Inlines
pLink = pReferenceLink

many1Till p q = do
  x <- p
  xs <- manyTill p q
  return (x:xs)

pBracketedInlines :: P Inlines
pBracketedInlines = try $
  char '[' *> (toInlines <$> many1Till pInline (char ']'))

mkRefLink :: Inlines -> Inline
mkRefLink ils = Link (Label ils)
                (Ref{ key = Key ils , fallback = literal "[" <> ils <> literal "]" })

pReferenceLink :: P Inlines
pReferenceLink = try $ do
  (Link lab x) <- mkRefLink <$> pBracketedInlines
  s <- many spaceChar
  (k, fall) <- option (key x, fallback x) $ try $ do
                   s <- option mempty $ sp <$ many1 spaceChar
                   (Link _ y) <- mkRefLink <$> pBracketedInlines
                   let k' = if key y == Key mempty then key x else key y
                   let f' = fallback x <> s <> fallback y
                   return (k',f')
  return $ inline $ Link lab Ref{ key = k, fallback = fall }

pTxt :: P Inlines
pTxt = do
  x <- letter
  let txtchar = letter <|> (try $ char '_' <* lookAhead txtchar)
  xs <- many txtchar
  return $ literal $ T.pack (x:xs)

pInlinesBetween :: P a -> P b -> P Inlines
pInlinesBetween start end =
  mconcat <$> try (start *> manyTill pInline end)

pFours :: P Inlines
pFours = try $ do -- four or more *s or _s, to avoid blowup parsing emph/strong
  x <- (char '*' <|> char '_')
  y <- char x
  z <- char x
  rest <- many1 (char x)
  return $ literal $ T.pack $ x : y : z : rest

pEmph :: P Inlines
pEmph = emph <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = char '*' *> notFollowedBy (spaceChar <|> newline)
          starEnd   = char '*'
          ulStart   = char '_' *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = char '_'

pStrong :: P Inlines
pStrong = strong <$>
  (pInlinesBetween starStart starEnd <|> pInlinesBetween ulStart ulEnd)
    where starStart = string "**" *> notFollowedBy (spaceChar <|> newline)
          starEnd   = try (string "**")
          ulStart   = string "__" *> notFollowedBy (spaceChar <|> newline)
          ulEnd     = try (string "__")

trimInlines :: Inlines -> Inlines
trimInlines = Inlines . trimr . triml . unInlines
  where triml ils = case viewl ils of
                    EmptyL    -> ils
                    (Sp :< x) -> triml x
                    _         -> ils
        trimr ils = case viewr ils of
                    EmptyR    -> ils
                    (x :> Sp) -> trimr x
                    _         -> ils

pDoc :: P Blocks
pDoc = skipMany pNewline *> pBlocks <* eof >>= resolveRefs

resolveRefs :: Blocks -> P Blocks
resolveRefs bs = do
  refs <- sReferences <$> getState
  return $ transformBi (handleRef refs) bs

handleRef :: M.Map Key Source -> Inlines -> Inlines
handleRef refs (Inlines xs) = Inlines $ F.foldMap go xs
  where go (Link  lab Ref{ key = k, fallback = ils }) =
          case M.lookup k refs of
                 Just s  -> singleton $ Link lab s
                 Nothing -> unInlines ils
        go (Image lab Ref{ key = k, fallback = ils }) =
          case M.lookup k refs of
                 Just s  -> singleton $ Image lab s
                 Nothing -> unInlines ils
        go x = singleton x

pBlocks :: P Blocks
pBlocks = mconcat <$> pBlock `sepEndBy` pNewlines

pBlock :: P Blocks
pBlock = choice [pQuote, pCode, pHrule, pList, pReference, pHeader, pPara]

pBlank :: P Blocks
pBlank = mempty <$ pNewlines

pPara :: P Blocks
pPara = para <$> pInlines

pQuote :: P Blocks
pQuote = quote <$> try (quoteStart
   *> withBlockSep quoteStart (withEndline (optional quoteStart) pBlocks))
    where quoteStart = try $ nonindentSpace *> char '>'

pHeader :: P Blocks
pHeader = pHeaderSetext <|> pHeaderATX

setextChar :: P Char
setextChar = char '=' <|> char '-'

pHeaderSetext :: P Blocks
pHeaderSetext = try $ do
  -- lookahead to speed up parsing
  lookAhead $ skipMany nonnl *> pNewline *> setextChar
  ils <- toInlines <$> many1Till pInline newline
  c <- setextChar
  count 2 $ char c
  let level = if c == '=' then 1 else 2
  lookAhead spnl
  return $ header level ils

pHeaderATX :: P Blocks
pHeaderATX = try $ do
  level <- Prelude.length <$> many1 (char '#')
  sps
  let closeATX = try $ skipMany (char '#') *> lookAhead spnl
  header level <$> toInlines <$> many1Till pInline closeATX

pList :: P Blocks
pList = pBulletList <|> pOrderedList

pBulletList :: P Blocks
pBulletList = do
  (tight, bs) <- listSep (pListItem bullet)
  return $ if tight
              then bulletListTight bs
              else bulletListLoose bs

pOrderedList :: P Blocks
pOrderedList = do
  (tight, bs) <- listSep (pListItem enum)
  return $ if tight
              then orderedListTight bs
              else orderedListLoose bs

listSep :: P Blocks -> P (Bool, [Blocks])
listSep p = do
  x <- p
  res <- many $ try $ do m <- pNewlines
                         n <- p
                         return (m, n)
  let (ts, xs) = unzip res
  return (all (== 1) ts, x:xs)

pListItem :: P a -> P Blocks
pListItem start = try $ start *> withBlockSep (indentSpace <|> lookAhead spnl)
  (withEndline (notFollowedBy $ skipMany spaceChar *> listStart) pBlocks)

listStart :: P Char
listStart = bullet <|> enum

bullet :: P Char
bullet = try $ nonindentSpace *> oneOf "-+*" <* spaceChar

enum :: P Char
enum = try $ nonindentSpace *> (digit <|> char '#') <* char '.' <* spaceChar

indentSpace :: P ()
indentSpace = try $  (count 4 (char ' ') >> return ())
                 <|> (char '\t' >> return ())

nonindentSpace :: P ()
nonindentSpace = option () $ onesp *> option () onesp *> option () onesp
  where onesp = () <$ char ' '

anyLine :: P Text
anyLine = T.pack <$> many nonnl

pCode :: P Blocks
pCode  = try $ do
  indentSpace
  x <- anyLine
  pNewline
  xs <- sepBy' ((indentSpace <|> lookAhead spnl) *> anyLine) pNewline
  return $ code $ T.unlines $ Prelude.reverse $ dropWhile T.null
         $ Prelude.reverse (x:xs)

pHrule :: P Blocks
pHrule = try $ do
  sps
  c <- satisfy $ \x -> x == '*' || x == '-' || x == '_'
  count 2 $ sps *> char c
  skipMany $ sps *> char c
  lookAhead spnl
  return hrule

sepBy' :: P a -> P b -> P [a]
sepBy' p sep = do
  x <- p
  xs <- many $ try (sep *> p)
  return (x:xs)

pReference :: P Blocks
pReference = try $ do
  nonindentSpace
  key <- Key <$> pBracketedInlines
  char ':'
  spOptNl
  loc <- T.pack <$> many1 (satisfy $ \c -> c /= ' ' && c /= '\n' && c /= '\t')
  spOptNl
  tit <- option "" pRefTitle
  let src = Source{ location = escapeURI loc, title = tit }
  modifyState $ \st -> st{ sReferences = M.insert key src $ sReferences st }
  return mempty

escapeURI :: Text -> Text
escapeURI = T.pack . escapeURIString isAllowedInURI . B8.unpack . E.encodeUtf8

pRefTitle :: P Text
pRefTitle =  pRefTitleWith '\'' '\''
         <|> pRefTitleWith '"' '"'
         <|> pRefTitleWith '(' ')'
  where pRefTitleWith start end = T.pack <$> (char start *> manyTill nonnl
             (try $ char end *> lookAhead (() <$ spnl <|> eof)))
