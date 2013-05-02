
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default ( Default(..) )
import Data.Monoid  ( (<>) )
import Data.Boolean

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser ( window, document, getElementById, prompt, alert )
import Language.Sunroof.JS.JQuery
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

import Parser
import Types

main :: IO ()
main = do
  putStrLn "SERVER RUNNING!"
  sunroofServer def $ \doc -> do
    treeStore <- rsyncJS doc $ newJSRef (cast $ nullJS) :: IO (JSRef JSTree)
    upstream <- newUplink doc
    downstream <- rsyncJS doc $ newChan
    asyncJS doc $ initialize upstream
    
    nodeWidth <- rsyncJS doc $ nodeWidth
    nodeHeight <- rsyncJS doc $ nodeHeight
    drawNode <- rsyncJS doc $ drawNode
    
    nodeBoxX <- rsyncJS doc $ function $ \(c, jsT) -> do
      let t = match jsT
      nodeBoxW <- nodeBoxWidth (c, treeNode t)
      nodeW    <- nodeWidth $$ (c, jsT)
      return $ nodeW / 2 - nodeBoxW / 2
    
    displayError <- rsyncJS doc $ function $ \s -> do
      resultBox <- jq "#formula-result"
      resultBox # removeClass "btn-success"
      resultBox # removeClass "btn-warning"
      resultBox # addClass "btn-danger"
      resultBox # setText s
      canvas <- jq "#canvas"
      let wh = 100 :: JSNumber
      canvas # setAttr "width" (cast $ wh)
      canvas # setAttr "height" (cast $ wh)
      c <- getTreeCanvas
      c # clearRect (0,0) (wh, wh)
      return ()
    
    displayValue <- rsyncJS doc $ function $ \(res :: JSNumber) -> do
      resultBox <- jq "#formula-result"
      resultBox # removeClass "btn-danger"
      resultBox # removeClass "btn-warning"
      resultBox # addClass "btn-success"
      resultBox # setText (cast res)
      return ()
    
    displayMathError <- rsyncJS doc $ function $ \err -> do
      resultBox <- jq "#formula-result"
      resultBox # removeClass "btn-danger"
      resultBox # removeClass "btn-success"
      resultBox # addClass "btn-warning"
      resultBox # setText err
      return ()
    
    displayResult <- rsyncJS doc $ function $ \res -> do
      let msg = match res
      caseB (msgType msg) 
        [ ((==* "error"), displayMathError $$ (cast $ msgContent msg))
        , ((==* "result"), displayValue $$ (cast $ msgContent msg))
        ] (return ())
      return ()
    
    drawTree <- rsyncJS doc $ function $ \t -> do
      canvas <- jq "#canvas"
      c <- getTreeCanvas
      w <- nodeWidth $$ (c, t)
      h <- nodeHeight $$ (c, t)
      canvas # setAttr "width" (cast w)
      canvas # setAttr "height" (cast h)
      c # clearRect (0,0) (w, h)
      c # setFont (cast fontSize <> "px monospace" )
      drawNode $$ (c, t)
      displayResult $$ (treeResult $ match t)
    
    asyncJS doc $ forkJS $ loop () $ \ () -> do
      jsMsg <- readChan downstream
      c <- getTreeCanvas
      let msg = match jsMsg
      liftJS $ caseB (msgType msg) 
        [ ((==* "math"), do
            writeJSRef (cast $ msgContent msg) treeStore
            drawTree $$ (cast $ msgContent msg)
          )
        , ((==* "error"), do
            writeJSRef (cast $ nullJS) treeStore
            displayError $$ (cast $ msgContent msg)
          )
        ] (return ())
      return ()
    
    mainServerLoop doc upstream downstream

mainServerLoop :: SunroofEngine 
               -> Uplink JSString 
               -> JSChan JSMessage
               -> IO ()
mainServerLoop doc upstream downstream = do
  formula <- getUplink upstream
  case parseMathE formula of
    Left err -> asyncJS doc $ do
      msg <- jsMessage "error" (js err)
      writeChan msg downstream :: JSA ()
    Right e  -> asyncJS doc $ do
      jsE <- fromMathE e
      msg <- jsMessage "math" jsE
      writeChan msg downstream :: JSA ()
  mainServerLoop doc upstream downstream

fromMathE :: MathE -> JSA JSTree
fromMathE m@(NumE d) = do
  res <- jsResult $ evalM m
  emptyArr <- empty
  jsTree (string $ show d) emptyArr res
fromMathE m@(OpE e1 op e2) = do
  res <- jsResult $ evalM m
  tl <- fromMathE e1
  tr <- fromMathE e2
  cs <- newArray (tl, tr)
  jsTree (js op) cs res
fromMathE m@(FunE f e) = do
  res <- jsResult $ evalM m
  t <- fromMathE e
  cs <- newArray (t)
  jsTree (string f) cs res

createMathJS :: MathE -> [Int] -> (JSNumber -> JSNumber)
createMathJS _ [] = id
createMathJS (NumE d) _ = const $ js d
createMathJS m@(FunE f e) (n:ns) 
  | n == 0    = \k -> funJS f (createMathJS e ns k)
  | otherwise = const $ evalMathJS m
createMathJS m@(OpE e1 o e2) (n:ns)
  | n == 0    = \k -> opJS o (createMathJS e1 ns k) (evalMathJS e2)
  | n == 1    = \k -> opJS o (evalMathJS e1) (createMathJS e2 ns k)
  | otherwise = const $ evalMathJS m

evalMathJS :: MathE -> JSNumber
evalMathJS m = case evalM m of
  Left err -> cast $ object "NaN"
  Right d -> js $ d

opJS :: Char -> (JSNumber -> JSNumber -> JSNumber)
opJS o = case o of
  '+' -> (+)
  '-' -> (-)
  '*' -> (*)
  '/' -> (/)
  --op  -> errorE $ "Unknown operator: " ++ [op]

funJS :: String -> (JSNumber -> JSNumber)
funJS f = case f of
  "log" -> log
  "cos" -> cos
  "sin" -> sin
  "tan" -> tan
  --fun -> return $ \_ -> errorE $ "Undefined function '" ++ fun ++ "'!"

-- General Event Handling --------------------------------------

on' :: (SunroofArgument a, Sunroof a) => JSString -> (a -> JSB ()) -> JSObject -> JS t ()
on' evt handler o = on evt (cast nullJS) handler o

onFormulaKeyUp :: Uplink JSString -> JSObject -> JS t ()
onFormulaKeyUp upstream _ = do
  formula <- jq "#formula" >>= attr' "value" 
  putUplink formula upstream

getTreeCanvas :: JS t JSCanvas
getTreeCanvas = (document # getElementById "canvas") >>= getContext "2d"

initialize :: Uplink JSString -> JSA ()
initialize upstream = do
  --jqWin <- jq $ cast window
  --jqWin # on' "resize" onWindowResize
  formulaInput <- jq "#formula"
  formulaInput # on' "keyup" (onFormulaKeyUp upstream)

-- Render ------------------------------------------------------

fontSize :: JSNumber
fontSize = js (14 :: Int)

margin :: JSNumber
margin = js (5 :: Int)

boxPadding :: JSNumber
boxPadding = js (5 :: Int)

boxLineWidth :: JSNumber
boxLineWidth = js (2 :: Int)

-- Draw a link between two coordinates.
drawLink :: (JSNumber, JSNumber) -> (JSNumber, JSNumber) -> JSCanvas -> JSA ()
drawLink (x1, y1) (x2, y2) c = do
  c # save
  c # setLineWidth boxLineWidth
  c # setStrokeStyle "#000000"
  c # beginPath
  c # moveTo (x1, y1)
  c # lineTo (x2, y2)
  c # closePath
  c # stroke
  c # restore

nodeBoxWidth :: (JSCanvas, JSString) -> JSA JSNumber
nodeBoxWidth (c, text) = do
  metric <- c # measureText text
  return $ 2 * boxPadding
         + fontSize + (metric ! width) 
         + 2 * margin

nodeBoxHeight :: (JSCanvas, JSString) -> JSA JSNumber
nodeBoxHeight (c, text) = do
  return $ 2 * boxPadding
         + fontSize
         + 2 * margin

drawNodeBox :: (JSCanvas, JSString) -> JSA ()
drawNodeBox (c, text) = do
  c # save
  c # translate (margin, margin)
  let r = boxPadding + fontSize / 2
  c # setLineWidth boxLineWidth
  metric <- c # measureText text
  c # beginPath
  c # arc (r, r) r (0.5 * pi, 1.5 * pi)
  c # lineTo (r + (metric ! width), 0)
  c # arc (r + (metric ! width), r) r (1.5 * pi, 0.5 * pi)
  c # lineTo (r, 2 * r)
  c # closePath
  c # save
  c # setFillStyle "#ffffff"
  c # fill
  c # restore
  c # stroke
  c # setTextBaseline "middle"
  c # setTextAlign "center"
  c # setFont ("bold " <> cast fontSize <> "px monospace")
  c # fillText text (r + (metric ! width) / 2, r)
  c # restore

childWidth :: (JSFunction (JSCanvas, JSTree) JSNumber, JSCanvas, JSArray JSTree) -> JSA JSNumber
childWidth (nodeWidth, c, children) = do
  -- Width of the children
  let foldFun e w = do
        eWidth <- nodeWidth $$ (c, e)
        return $ eWidth + w
  foldArray foldFun 0 children

nodeWidth :: JSA (JSFunction (JSCanvas, JSTree) JSNumber)
nodeWidth = fixJS $ \nodeWidth -> function $ \(c, jsT) -> do
  let t = match jsT
  -- Width of the node text
  nodeW <- nodeBoxWidth (c, treeNode t)
  -- Width of the children
  childW <- childWidth (nodeWidth, c, treeChildren t)
  -- Overall width
  return $ maxB nodeW childW

nodeHeight :: JSA (JSFunction (JSCanvas, JSTree) JSNumber)
nodeHeight = fixJS $ \nodeHeight -> function $ \(c, jsT) -> do
  let t = match jsT
  -- Height of the node text
  nodeH <- nodeBoxHeight (c, treeNode t)
  -- Height of the children
  let foldFun e h = do
        eHeight <- nodeHeight $$ (c, e)
        return $ maxB eHeight h
  childH <- foldArray foldFun 0 (treeChildren t)
  -- Overall height
  return $ nodeH + childH

drawNode :: JSA (JSFunction (JSCanvas, JSTree) ())
drawNode = fixJS $ \drawNode -> function $ \(c, jsT) -> do
  let t = match jsT
  -- Compile Functions:
  nodeWidthF <- nodeWidth
  -- Render node text
  nodeBoxW <- nodeBoxWidth (c, treeNode t)
  nodeW    <- nodeWidthF $$ (c, jsT)
  nodeBoxH <- nodeBoxHeight (c, treeNode t)
  childW   <- childWidth (nodeWidthF, c, treeChildren t)
  let nodeLocX = nodeW / 2 - nodeBoxW / 2
  c # save
  c # translate (nodeLocX, 0)
  drawNodeBox (c, treeNode t)
  c # restore
  -- Render children
  let offset = ifB (childW <* nodeW) (nodeW / 2 - childW / 2) (0)
  let foldFun node offset = do
        c # save
        childW <- nodeWidthF $$ (c, node)
        c # drawLink (nodeLocX + nodeBoxW / 2, nodeBoxH - margin) 
                     (offset + childW / 2, nodeBoxH + margin)
        c # translate (offset, nodeBoxH)
        drawNode $$ (c, node)
        c # restore
        return $ childW + offset
  _ <- foldArray foldFun offset (treeChildren t)
  return ()

foldArray :: (SunroofArgument a, Sunroof a, Sunroof b) 
          => (a -> b -> JSA b) -> b -> JSArray a -> JSA b
foldArray f e l = do
  jsF <- function (uncurry f)
  accum <- newJSRef e
  l # forEach $ \x -> modifyJSRef (\v -> jsF $$ (x, v)) accum
  readJSRef accum



















