{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc2.Reader.TeXMath ( texMathToPandoc ) where

import Text.Pandoc2.Definition
import Text.Pandoc2.Builder
import Text.TeXMath.Types
import Text.TeXMath.Parser
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad
import Data.Monoid

texMathToPandoc :: Text -> Either String Inlines
texMathToPandoc inp = inp `seq`
  case parseFormula (T.unpack inp) of
         Left err    -> Left err
         Right exps  -> case expsToInlines exps of
                             Nothing  -> Left "Formula too complex for [Inline]"
                             Just r   -> Right r

expsToInlines :: [Exp] -> Maybe Inlines
expsToInlines =
  foldM (\ils e -> liftM2 (<>) (return ils) (expToInlines e)) mempty

str :: String -> Inlines
str = txt . T.pack

expToInlines :: Exp -> Maybe Inlines
expToInlines (ENumber s) = return $ str s
expToInlines (EIdentifier s) = return $ emph $ str s
expToInlines (EMathOperator s) = return $ str s
expToInlines (ESymbol t s) = return $ addSpace t $ str s
  where addSpace Op x  = x <> thinspace
        addSpace Bin x = medspace <> x <> medspace
        addSpace Rel x = widespace <> x <> widespace
        addSpace Pun x = x <> thinspace
        addSpace _ x   = x
        thinspace      = txt "\x2006"
        medspace       = txt "\x2005"
        widespace      = txt "\x2004"
expToInlines (EStretchy x) = expToInlines x
expToInlines (EGrouped xs) = expsToInlines xs
expToInlines (ESpace _) = return $ str " "
expToInlines (EBinary _ _ _) = fail "EBinary not implemented"
expToInlines (ESub x y) = do
  x' <- expToInlines x
  y' <- expToInlines y
  return $ x' <> subscript y'
expToInlines (ESuper x y) = do
  x' <- expToInlines x
  y' <- expToInlines y
  return $ x' <> superscript y'
expToInlines (ESubsup x y z) = do
  x' <- expToInlines x
  y' <- expToInlines y
  z' <- expToInlines z
  return $ x' <> subscript y' <> superscript z'
expToInlines (EDown x y) = expToInlines (ESub x y)
expToInlines (EUp x y) = expToInlines (ESuper x y)
expToInlines (EDownup x y z) = expToInlines (ESubsup x y z)
expToInlines (EText "normal" x) = return $ str x
expToInlines (EText "bold" x) = return $ strong $ str x
expToInlines (EText "monospace" x) = return $ verbatim $ T.pack x
expToInlines (EText "italic" x) = return $ emph $ str x
expToInlines (EText _ x) = return $ str x
expToInlines (EOver (EGrouped [EIdentifier [c]]) (ESymbol Accent [accent])) =
    case accent of
         '\x203E' -> return $ emph $ str [c,'\x0304']  -- bar
         '\x00B4' -> return $ emph $ str [c,'\x0301']  -- acute
         '\x0060' -> return $ emph $ str [c,'\x0300']  -- grave
         '\x02D8' -> return $ emph $ str [c,'\x0306']  -- breve
         '\x02C7' -> return $ emph $ str [c,'\x030C']  -- check
         '.'      -> return $ emph $ str [c,'\x0307']  -- dot
         '\x00B0' -> return $ emph $ str [c,'\x030A']  -- ring
         '\x20D7' -> return $ emph $ str [c,'\x20D7']  -- arrow right
         '\x20D6' -> return $ emph $ str [c,'\x20D6']  -- arrow left
         '\x005E' -> return $ emph $ str [c,'\x0302']  -- hat
         '\x0302' -> return $ emph $ str [c,'\x0302']  -- hat
         '~'      -> return $ emph $ str [c,'\x0303']  -- tilde
         _        -> fail "Accent not supported"
expToInlines _ = fail "Element not supported"


