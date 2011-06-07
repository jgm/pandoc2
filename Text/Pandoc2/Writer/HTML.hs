{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
    TypeSynonymInstances #-}
module Text.Pandoc2.Writer.HTML (docToHtml) where
import Text.Pandoc2.Definition
import Text.Pandoc2.Builder
import Text.Pandoc2.Shared
import Text.Pandoc2.Reader.TeXMath
import Text.Blaze
import Text.Blaze.Internal (HtmlM (Empty))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.List (intersperse)
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Text.TeXMath
import Text.XML.Light.Output

data WriterState = WriterState { wOptions     :: POptions
                               , wNotes       :: [Html]
                               , wIdentifiers :: M.Map Text Int
                               }

type W = State WriterState Html

instance Monoid W where
  mempty = return mempty
  mappend = liftM2 mappend

nl :: W
nl = return $ preEscapedText "\n"

docToHtml :: POptions -> Blocks -> Html
docToHtml opts bs =
  evalState goHtml WriterState{ wOptions = opts, wNotes   = [], wIdentifiers = M.empty }
    where goHtml = do body <- blocksToHtml bs
                      notes <- reverse . wNotes <$> get
                      spacer <- nl
                      let fnblock = H.div ! A.class_ "footnotes"
                                          $ H.hr <>
                                            (H.ol ! A.class_ "footnotes"
                                            $  spacer
                                            <> mconcat
                                                (intersperse spacer notes)
                                            <> spacer)
                      return $ body <> spacer <> if null notes
                                                    then mempty
                                                    else fnblock <> spacer

blocksToHtml :: Blocks -> W
blocksToHtml bs = foldM go mempty $ toItems bs
  where go a b = do x <- return a
                    y <- if isEmpty a then return mempty else nl
                    z <- blockToHtml b
                    return $ x <> y <> z
        isEmpty Empty = True
        isEmpty _     = False

blockToHtml :: Block -> W
blockToHtml (Para ils) = H.p <$> inlinesToHtml ils
blockToHtml (Plain ils) = inlinesToHtml ils
blockToHtml (Quote bs) = H.blockquote <$> nl <> blocksToHtml bs
blockToHtml (List style bs) =
  let items = F.foldMap (\b -> (H.li <$> blocksToHtml b) <> nl) bs
      addStart 1 t = t
      addStart n t = t ! A.start (toValue n)
      addStyle DefaultStyle t = t
      addStyle sty t = t ! A.type_ (toValue $ case sty of
                                                   Decimal    -> "1" :: String
                                                   LowerAlpha -> "a"
                                                   UpperAlpha -> "A"
                                                   LowerRoman -> "i"
                                                   UpperRoman -> "I"
                                                   _          -> "1")
  in  case style of
           Bullet              -> H.ul <$> nl <> items
           Ordered start sty _ -> ol   <$> nl <> items
              where ol = addStart start $ addStyle sty $ H.ol
blockToHtml (Definitions items) = do
  let toTerm ils = inlinesToHtml ils
  let toDef bs   = (H.dd <$> blocksToHtml bs) <> nl
  let toItem (term, defs) = (H.dt <$> toTerm term)
                         <> nl <> (mconcat $ map toDef defs)
  H.dl <$> nl <> mconcat (map toItem items)
blockToHtml (Code attr t) = return $ addAttributes attr
                                   $ H.pre $ H.code $ toHtml t
blockToHtml (RawBlock (Format "html") t) = return $ preEscapedText t
blockToHtml (RawBlock _ _) = return mempty
blockToHtml (Header lev ils) = do
  let h = case lev of
             1 -> H.h1
             2 -> H.h2
             3 -> H.h3
             4 -> H.h4
             5 -> H.h5
             _ -> H.p
  opts <- wOptions <$> get
  ht <- if isEnabled Header_identifiers (optExtensions opts)
           then do
             let ident = inlinesToIdentifier ils
             idents <- wIdentifiers <$> get
             case M.lookup ident idents of
                  Nothing -> do
                    modify (\s -> s{ wIdentifiers = M.insert ident 1 idents })
                    return $ h ! A.id (toValue ident)
                  Just n  -> do
                    modify (\s -> s{ wIdentifiers = M.insert ident (n+1) idents })
                    return $ h ! A.id (toValue $ ident <> "-" <> show' n)
           else return h
  ht <$> inlinesToHtml ils
blockToHtml HRule = return H.hr

inlinesToHtml :: Inlines -> W
inlinesToHtml ils = mconcat <$> (mapM inlineToHtml $ toItems ils)

inlineToHtml :: Inline -> W
inlineToHtml (Txt x) = return $ toHtml x
inlineToHtml Sp      = return $ toHtml (" " :: L.Text)
inlineToHtml (Emph ils) = H.em <$> inlinesToHtml ils
inlineToHtml (Strong ils) = H.strong <$> inlinesToHtml ils
inlineToHtml (Subscript ils) = H.sub <$> inlinesToHtml ils
inlineToHtml (Superscript ils) = H.sup <$> inlinesToHtml ils
inlineToHtml (Strikeout ils) = H.del <$> inlinesToHtml ils
inlineToHtml (Link (Label lab) src@Source{}) = do
  let tit = title src
  x <- (H.a ! A.href (toValue $ location src)) <$> inlinesToHtml lab
  return $ if T.null tit then x else x ! A.title (toValue $ title src)
inlineToHtml (Image (Label lab) src@Source{}) = return $
  H.img ! A.src (toValue $ location src) ! A.title (toValue $ title src)
        ! A.alt (toValue $ textify lab)
inlineToHtml LineBreak = return $ H.br
inlineToHtml (Math sty t) = do
  mathMethod <- optMathMethod . wOptions <$> get
  case mathMethod of
    MathML    -> return $ toMathML sty t
    PlainMath -> case texMathToPandoc t of
                        Left _    ->
                          if sty == InlineMath
                             then return $ H.span ! A.class_ "math" $
                                    toHtml ("$" <> t <> "$")
                             else return $ H.span ! A.class_ "math" $
                                    toHtml ("$$" <> t <> "$$")
                        Right ils ->
                          if sty == InlineMath
                             then inlinesToHtml ils
                             else inlinesToHtml
                                  (lineBreak <> ils <> lineBreak)
    TeXMath   -> return $ h ! A.class_ "math" $ toHtml t
      where h = if sty == InlineMath then H.span else H.div
inlineToHtml (RawInline (Format "html") t) = return $ preEscapedText t
inlineToHtml (RawInline _ _) = return $ mempty
inlineToHtml (Verbatim _attr t) = return $ H.code $ toHtml t
inlineToHtml (Quoted SingleQuoted ils) = do
  xs <- inlinesToHtml ils
  return $ "\8216" <> xs <> "\8217"
inlineToHtml (Quoted DoubleQuoted ils) = do
  xs <- inlinesToHtml ils
  return $ "\8220" <> xs <> "\8221"
inlineToHtml (Note bs) = do
  notes <- wNotes <$> get
  let nextnum = show $ length notes + 1
  let refid = "fnref" ++ nextnum
  let noteid = "fn" ++ nextnum
  contents <- blocksToHtml $ addBacklink refid bs
  let marker = H.sup
             $ H.a ! A.id (toValue refid)
                   ! A.href (toValue $ '#':noteid)
                   ! A.class_ "footnoteRef"
                   $ toHtml nextnum
  let fn = H.li ! A.id (toValue noteid)
                ! A.href (toValue $ '#':refid)
                ! A.class_ "footnote"
                $ contents
  modify $ \st -> st{ wNotes = fn : notes }
  return marker

addBacklink :: String -> Blocks -> Blocks
addBacklink refid bs =
  let back = rawInline (Format "html")
           $ "<a href=\"" <> T.pack ('#':refid) <> "\" class=\"footnoteBackLink\" title=\"Back to text\">&#8617;</a>"
  in case reverse (toItems bs) of
       (Para ils : xs)   -> fromItems $ reverse $ Para (ils <+> back) : xs
       (Plain xls : xs)  -> fromItems $ reverse $ Plain (xls <+> back) : xs
       xs                -> fromItems $ reverse $ Plain back : xs

addAttributes :: Attr -> Html -> Html
addAttributes (Attr []) h = h
addAttributes (Attr attr) h =
  foldl (\acc (k,v) -> acc ! (customAttribute $ textTag k) (toValue v)) h
  $ consolidateAttr attr
    where consolidateAttr = foldl go []
          go acc (k,v) = case lookup k acc of
                              Nothing -> (k,v):acc
                              Just v' -> (k, T.unwords [v',v]):
                                           [(x,y) | (x,y) <- acc, x /= k]

toMathML :: MathType -> Text -> Html
toMathML t m =
  let dt = if t == InlineMath
              then DisplayInline
              else DisplayBlock
      conf = useShortEmptyTags (const False) defaultConfigPP
  in case texMathToMathML dt (T.unpack m) of
      Right r -> preEscapedString $ ppcElement conf r
      Left  _ -> H.span ! A.class_ "math" $ toHtml m
