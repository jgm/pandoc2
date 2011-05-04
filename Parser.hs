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
import Control.Applicative ((<$>), (<$), (*>), (<*))

{-
 - - References:  use generics to extract, instead of doing two passes?
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
                , sReferences = M.fromList [(Key (txt "teST"), Source{ location = "/url", title = "tit" })] -- TODO M.empty
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
pEndline = sp <$
  try (newline *> (getState >>= sequenceA . sEndline) *> notFollowedBy spnl)

spnl :: P Char
spnl = try $ skipMany spaceChar *> newline

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
pInline = choice [ pSp, pTxt, pEndline, pLink ]

pInlines :: P Inlines
pInlines = trimInlines . mconcat <$> many1 pInline

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
pBracketedInlines = try $ char '[' *> (mconcat <$> many1Till pInline (char ']'))

mkRefLink :: Inlines -> Inline
mkRefLink ils = Link (Label $ trimInlines ils)
                     (Ref{ key = Key ils
                         , fallback = literal "[" <> ils <> literal "]" })

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
pTxt = literal . T.pack <$> many1 letter

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
pDoc = pBlocks >>= resolveRefs

resolveRefs :: Blocks -> P Blocks
resolveRefs bs = do
  refs <- sReferences <$> getState
  return $ bottomUp (handleRef refs) bs

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
pBlocks = mconcat <$> sepEndBy pBlock pNewlines

pBlock :: P Blocks
pBlock = choice [pQuote, pCode, pList, pPara]

pPara :: P Blocks
pPara = para <$> pInlines

pQuote :: P Blocks
pQuote = quote <$> try (quoteStart
   *> withBlockSep quoteStart (withEndline (optional quoteStart) pBlocks))
    where quoteStart = char '>'

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
pListItem start = try $ start *> withBlockSep indentSpace
  (withEndline (notFollowedBy $ optional indentSpace *> listStart) pBlocks)

listStart :: P Char
listStart = bullet <|> enum

bullet :: P Char
bullet = try $ nonindentSpace *> oneOf "-+*" <* spaceChar

enum :: P Char
enum = try $ nonindentSpace *> (digit <|> char '#') <* char '.' <* spaceChar

indentSpace :: P ()
indentSpace = try $  (count 4 (char ' ') >> return ())
                 <|> (char '\t' >> return ())
                 <|> (lookAhead spnl >> return ())

nonindentSpace :: P ()
nonindentSpace = option () $ onesp *> option () onesp *> option () onesp
  where onesp = () <$ char ' '

anyLine :: P Text
anyLine = T.pack <$> many (satisfy (/='\n'))

pCode :: P Blocks
pCode  = toCodeBlock <$> sepBy1 (indentSpace *> anyLine) pNewline
  where toCodeBlock = code . T.unlines . Prelude.reverse
                    . dropWhile T.null . Prelude.reverse
