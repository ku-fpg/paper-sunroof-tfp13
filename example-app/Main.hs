
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Default ( Default(..) )
import Data.Monoid  ( (<>) )
import Data.Boolean ( BooleanOf, IfB(..), EqB(..), maxB )
import Data.Attoparsec.Char8 
  ( Parser, parseOnly
  , takeWhile1, satisfy
  , skipSpace, double )
import Data.Char ( isLetter )
import qualified Data.ByteString.Char8 as BS

import Control.Applicative hiding ( empty )

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser ( document, getElementById, prompt, alert )
--import Language.Sunroof.JS.JQuery
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

main :: IO ()
main = do
  putStrLn "SERVER RUNNING!"
  sunroofServer def $ \doc -> do
    drawLink <- rsyncJS doc $ function drawLink
    renderOutline <- rsyncJS doc $ function renderOutline
    nodeWidth <- rsyncJS doc $ function nodeWidth
    nodeHeight <- rsyncJS doc $ function nodeHeight
    drawNode <- rsyncJS doc $ function drawNode
    renderMathE <- rsyncJS doc $ function renderMathE
    
    makeMathDouble <- rsyncJS doc $ function $ \d -> do
      widthF  <- function $ \c -> nodeWidth $$ (c, cast d, empty)
      heightF <- function $ \c -> nodeHeight $$ (c, cast d, empty)
      renderF <- function $ \c -> drawNode $$ (c, cast d, empty)
      tuple (widthF, heightF, renderF)
    
    makeMathOp <- rsyncJS doc $ function $ \(el, op, er) -> do
      children <- newArray (el, er)
      widthF  <- function $ \c -> nodeWidth $$ (c, op, children)
      heightF <- function $ \c -> nodeHeight $$ (c, op, children)
      renderF <- function $ \c -> drawNode $$ (c, op, children)
      tuple (widthF, heightF, renderF)
    
    makeMathFun <- rsyncJS doc $ function $ \(f, e) -> do
      children <- newArray (e)
      widthF  <- function $ \c -> nodeWidth $$ (c, f, children)
      heightF <- function $ \c -> nodeHeight $$ (c, f, children)
      renderF <- function $ \c -> drawNode $$ (c, f, children)
      tuple (widthF, heightF, renderF)
    
    let makeMathE :: MathE -> JSA JSMathE
        makeMathE (NumE d) = makeMathDouble $$ js d
        makeMathE (OpE e1 op e2) = do
          me1 <- makeMathE e1
          me2 <- makeMathE e2
          makeMathOp $$ (me1, js op, me2)
        makeMathE (FunE f e) = do
          me <- makeMathE e
          makeMathFun $$ (js f, me)
    
    case parseMathE "3 + 4 * log(5)" of
      Left err -> asyncJS doc $ alert $ js err
      Right e  -> asyncJS doc $ do
        jsE <- makeMathE e
        renderMathE $$ jsE
  

prog :: JSB ()
prog = do
  answer <- prompt "Please enter your expression:" ""
  ifB (nullJS ==* answer) (alert $ "Invalid! NULL!") $ do
    alert $ "ANSWER: " <> cast answer

-- Type --------------------------------------------------------

data MathE = NumE Double
           | OpE MathE Char MathE
           | FunE String MathE
           deriving Show

-- JSMathE -----------------------------------------------------

newtype JSMathE = JSMathE JSObject

instance Show JSMathE where
  show (JSMathE o) = show o

instance Sunroof JSMathE where
  box = JSMathE . box
  unbox (JSMathE o) = unbox o

type instance BooleanOf JSMathE = JSBool

instance IfB JSMathE where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSMathE where
  (JSMathE a) ==* (JSMathE b) = a ==* b

instance JSTuple JSMathE where
  type Internals JSMathE = ( JSFunction JSCanvas JSNumber -- Calc Width
                           , JSFunction JSCanvas JSNumber -- Calc Height
                           , JSFunction JSCanvas ()       -- Render
                           )
  match o = ( o ! "width", o ! "height", o ! "render" )
  tuple (w, h, render) = do
    o <- new "Object" ()
    o # "width"  := w
    o # "height" := h
    o # "render" := render
    return $ JSMathE o

-- Render ------------------------------------------------------

fontSize :: JSNumber
fontSize = js (14 :: Int)

borderSize :: JSNumber
borderSize = js (10 :: Int)

-- Draw a link between two coordinates.
drawLink :: (JSCanvas, JSNumber, JSNumber, JSNumber, JSNumber) -> JSA ()
drawLink (c, x1, y1, x2, y2) = do
  c # save
  c # beginPath
  c # moveTo (x1, y1)
  c # lineTo (x2, y2)
  c # closePath
  c # stroke
  c # restore

-- Draw a box around the given JSMathE.
renderOutline :: (JSMathE, JSCanvas) -> JSA ()
renderOutline (e, c) = do
  c # save
  let (wF, hF, rF) = match e
  w <- wF $$ c
  h <- hF $$ c
  c # setStrokeStyle "red"
  c # rect (0,0) (w,h)
  c # stroke
  c # restore

nodeWidth :: (JSCanvas, JSString, JSArray JSMathE) -> JSA JSNumber
nodeWidth (c, node, children) = do
  -- Width of the node text
  metric <- c # measureText node
  return $ 2 * borderSize + (metric ! width)
  -- Width of the children
  let foldFun e w = do
        let (widthF, heightF, renderF) = match e
        eWidth <- widthF $$ c
        return $ eWidth + w
  childW <- foldArray foldFun 0 children
  -- Overall width
  return $ maxB (2 * borderSize + (metric ! width)) childW

nodeHeight :: (JSCanvas, JSString, JSArray JSMathE) -> JSA JSNumber
nodeHeight (c, node, children) = do
  -- Height of the node text
  metric <- c # measureText node
  return $ 2 * borderSize + (metric ! width)
  -- Height of the children
  let foldFun e h = do
        let (widthF, heightF, renderF) = match e
        eHeight <- heightF $$ c
        return $ maxB eHeight h
  childH <- foldArray foldFun 0 children
  -- Overall height
  return $ borderSize + fontSize + childH

drawNode :: (JSCanvas, JSString, JSArray JSMathE) -> JSA ()
drawNode (c, node, children) = do
  -- Render node text
  c # setTextBaseline "top"
  c # setTextAlign "center"
  w <- nodeWidth (c, node, children)
  c # fillText node (w / 2, borderSize)
  -- Render children
  let foldFun e offset = do
        let (widthF, _, renderF) = match e
        c # save
        c # translate (offset, borderSize + fontSize)
        renderF $$ c
        c # restore
        childWidth <- widthF $$ c
        return $ childWidth + offset
  _ <- foldArray foldFun 0 children
  return ()

foldArray :: (SunroofArgument a, Sunroof a, Sunroof b) 
          => (a -> b -> JSA b) -> b -> JSArray a -> JSA b
foldArray f e l = do
  jsF <- function (uncurry f)
  accum <- newJSRef e
  l # forEach $ \x -> modifyJSRef (\v -> jsF $$ (x, v)) accum
  readJSRef accum

renderMathE :: JSMathE -> JSA ()
renderMathE e = do
  c <- (document # getElementById "canvas") >>= getContext "2d"
  c # setFont (cast fontSize <> "px Arial" )
  let (_, _, renderF) = match e
  c # fillText "Test" (0,0)
  renderF $$ c

-- Parser ------------------------------------------------------

scanFunName :: Parser String
scanFunName = BS.unpack <$> (skipSpace *> takeWhile1 isLetter)

scanNumber :: Parser Double
scanNumber = skipSpace *> double

scanOp :: [Char] -> Parser Char
scanOp ops = skipSpace *> satisfy (`elem` ops)

scanParenL :: Parser ()
scanParenL = skipSpace *> satisfy (== '(') *> return ()

scanParenR :: Parser ()
scanParenR = skipSpace *> satisfy (== ')') *> return ()

parseNumber :: Parser MathE
parseNumber = NumE <$> scanNumber

parseParen :: Parser MathE
parseParen = scanParenL *> parseExp <* scanParenR

parseFun :: Parser MathE
parseFun = FunE <$> scanFunName <*> parseParen

parseAddSub :: Parser MathE
parseAddSub = OpE <$> parseTerm <*> scanOp "+-" <*> parseExp

parseMulDiv :: Parser MathE
parseMulDiv = OpE <$> parseFactor <*> scanOp "*/" <*> parseTerm

parseExp :: Parser MathE
parseExp  =  parseAddSub
         <|> parseTerm

parseTerm :: Parser MathE
parseTerm  =  parseMulDiv
          <|> parseFactor

parseFactor :: Parser MathE
parseFactor  =  parseFun
            <|> parseParen
            <|> parseNumber

parseMathE :: String -> ErrorE MathE
parseMathE str = parseOnly (parseExp <* skipSpace) (BS.pack str)

-- Evaluation --------------------------------------------------

type ErrorE a = Either String a

errorE :: String -> ErrorE a
errorE = Left

opM :: Char -> ErrorE (Double -> Double -> ErrorE Double)
opM o = case o of
  '+' -> return (\d1 d2 -> return $ d1 + d2)
  '-' -> return (\d1 d2 -> return $ d1 - d2)
  '*' -> return (\d1 d2 -> return $ d1 * d2)
  '/' -> return (\d1 d2 -> if d2 == 0 
                              then errorE "Division by zero!" 
                              else return $ d1 / d2)
  op  -> errorE $ "Unknown operator: " ++ [op]

funM :: String -> ErrorE (Double -> ErrorE Double)
funM f = case f of
  "log" -> return $ return . log
  "cos" -> return $ return . cos
  "sin" -> return $ return . sin
  "tan" -> return $ return . tan

evalM :: MathE -> ErrorE Double
evalM (NumE d) = return d
evalM (OpE e1 o e2) = do
  d1 <- evalM e1
  d2 <- evalM e2
  op <- opM o
  d1 `op` d2
evalM (FunE f e) = do
  d <- evalM e
  fun <- funM f
  fun d

















