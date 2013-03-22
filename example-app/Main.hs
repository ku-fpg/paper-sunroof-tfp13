
{-# LANGUAGE OverloadedStrings #-}

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
    upstream <- newUplink doc
    downstream <- rsyncJS doc $ newChan
    asyncJS doc $ initialize upstream
    
    nodeWidth <- rsyncJS doc $ function nodeWidth
    nodeHeight <- rsyncJS doc $ function nodeHeight
    drawNode <- rsyncJS doc $ function drawNode
    --renderMathE <- rsyncJS doc $ function renderMathE
    
    makeMathDouble <- rsyncJS doc $ function $ \(d, res) -> do
      widthF  <- function $ \c -> nodeWidth $$ (c, cast d, empty)
      heightF <- function $ \c -> nodeHeight $$ (c, cast d, empty)
      renderF <- function $ \c -> drawNode $$ (c, cast d, empty)
      tuple (widthF, heightF, renderF, res)
    
    makeMathOp <- rsyncJS doc $ function $ \(el, op, er, res) -> do
      children <- newArray (el, er)
      widthF  <- function $ \c -> nodeWidth $$ (c, op, children)
      heightF <- function $ \c -> nodeHeight $$ (c, op, children)
      renderF <- function $ \c -> drawNode $$ (c, op, children)
      tuple (widthF, heightF, renderF, res)
    
    makeMathFun <- rsyncJS doc $ function $ \(f, e, res) -> do
      children <- newArray (e)
      widthF  <- function $ \c -> nodeWidth $$ (c, f, children)
      heightF <- function $ \c -> nodeHeight $$ (c, f, children)
      renderF <- function $ \c -> drawNode $$ (c, f, children)
      tuple (widthF, heightF, renderF, res)
    
    let makeMathE :: MathE -> JSA JSMathE
        makeMathE (NumE d) = do
          res <- jsErrorE $ Right d
          makeMathDouble $$ (js d, res)
        makeMathE m@(OpE e1 op e2) = do
          me1 <- makeMathE e1
          me2 <- makeMathE e2
          res <- jsErrorE $ evalM m
          makeMathOp $$ (me1, js op, me2, res)
        makeMathE m@(FunE f e) = do
          me <- makeMathE e
          res <- jsErrorE $ evalM m
          makeMathFun $$ (js f, me, res)
    
    asyncJS doc $ forkJS $ updateCanvasLoop downstream
    
    mainServerLoop doc upstream downstream makeMathE

mainServerLoop :: SunroofEngine 
               -> Uplink JSString 
               -> JSChan JSMessage
               -> (MathE -> JSA JSMathE)
               -> IO ()
mainServerLoop doc upstream downstream convertMathE = do
  formula <- getUplink upstream
  case parseMathE formula of
    Left err -> asyncJS doc $ do
      msg <- tuple ("error", cast $ js err)
      writeChan msg downstream :: JSA ()
    Right e  -> asyncJS doc $ do
      jsE <- convertMathE e
      msg <- tuple ("math", cast $ jsE)
      writeChan msg downstream :: JSA ()
  mainServerLoop doc upstream downstream convertMathE
  
jsErrorE :: ErrorE Double -> JSA JSMessage
jsErrorE (Left err) = tuple ("error", cast $ js err)
jsErrorE (Right v)  = tuple ("result", cast $ js v)

-- General Event Handling --------------------------------------

on' :: (SunroofArgument a, Sunroof a) => JSString -> (a -> JSA ()) -> JSObject -> JS t ()
on' evt handler o = on evt (cast nullJS) handler o

onFormulaKeyUp :: Uplink JSString -> JSObject -> JSA ()
onFormulaKeyUp upstream _ = do
  formula <- jq "#formula" >>= attr' "value" 
  putUplink formula upstream

updateCanvasLoop :: JSChan JSMessage -> JSB ()
updateCanvasLoop downstream = loop () $ \ () -> do
  msg <- readChan downstream
  liftJS $ caseB (fst $ match msg) 
    [ ((==* "math"), renderMathE (cast $ snd $ match msg))
    , ((==* "error"), displayError (cast $ snd $ match msg))
    ] (return ())
  return ()

updateCanvasSize :: JSMathE -> JSA ()
updateCanvasSize e = do
  let (widthF, heightF, _, _) = match e
  canvas <- jq "#canvas"
  c <- (document # getElementById "canvas") >>= getContext "2d"
  w <- widthF $$ c
  h <- heightF $$ c
  canvas # setAttr "width" (cast w)
  canvas # setAttr "height" (cast h)
  return ()

onWindowResize :: JSObject -> JSA ()
onWindowResize _ = do
  bodyW <- jq "body" >>= innerWidth
  canvas <- jq "#canvas"
  --canvas # setAttr "width" (cast bodyW)
  return ()

initialize :: Uplink JSString -> JSA ()
initialize upstream = do
  onWindowResize nullJS
  jqWin <- jq $ cast window
  jqWin # on' "resize" onWindowResize
  formulaInput <- jq "#formula"
  formulaInput # on' "keyup" (onFormulaKeyUp upstream)
    --canvas # setAttr "height" "h"

renderMathE :: JSMathE -> JSA ()
renderMathE e = do
  updateCanvasSize e
  c <- (document # getElementById "canvas") >>= getContext "2d"
  w <- jq "#canvas" >>= attr' "width"
  h <- jq "#canvas" >>= attr' "height"
  c # clearRect (0,0) (cast w, cast h)
  c # setFont (cast fontSize <> "px monospace" )
  let (_, _, renderF, res) = match e
  renderF $$ c
  displayResult res

displayError :: JSString -> JSA ()
displayError s = do
  resultBox <- jq "#formula-result"
  resultBox # removeClass "btn-success"
  resultBox # removeClass "btn-warning"
  resultBox # addClass "btn-danger"
  resultBox # setText s
  return ()

displayValue :: JSNumber -> JSA ()
displayValue res = do
  resultBox <- jq "#formula-result"
  resultBox # removeClass "btn-danger"
  resultBox # removeClass "btn-warning"
  resultBox # addClass "btn-success"
  resultBox # setText (cast res)
  return ()

displayMathError :: JSString -> JSA ()
displayMathError err = do
  resultBox <- jq "#formula-result"
  resultBox # removeClass "btn-danger"
  resultBox # removeClass "btn-success"
  resultBox # addClass "btn-warning"
  resultBox # setText err
  return ()

displayResult :: JSMessage -> JSA ()
displayResult res = do
  caseB (fst $ match res) 
    [ ((==* "error"), displayMathError (cast $ snd $ match res))
    , ((==* "result"), displayValue (cast $ snd $ match res))
    ] (return ())
  return ()

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

-- Draw a box around the given JSMathE.
renderOutline :: JSMathE -> JSCanvas -> JSA ()
renderOutline e c = do
  c # save
  let (wF, hF, rF, _) = match e
  w <- wF $$ c
  h <- hF $$ c
  c # setStrokeStyle "#ff0000"
  c # rect (0,0) (w,h)
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
  c # setFillStyle "#ff0000"
  c # fill
  c # restore
  c # stroke
  c # setTextBaseline "middle"
  c # setTextAlign "center"
  c # setFont ("bold " <> cast fontSize <> "px monospace")
  c # fillText text (r + (metric ! width) / 2, r)
  c # restore

childWidth :: (JSCanvas, JSArray JSMathE) -> JSA JSNumber
childWidth (c, children) = do
  -- Width of the children
  let foldFun e w = do
        let (widthF, heightF, renderF, _) = match e
        eWidth <- widthF $$ c
        return $ eWidth + w
  foldArray foldFun 0 children

nodeWidth :: (JSCanvas, JSString, JSArray JSMathE) -> JSA JSNumber
nodeWidth (c, node, children) = do
  -- Width of the node text
  nodeW <- nodeBoxWidth (c, node)
  -- Width of the children
  childW <- childWidth (c, children)
  -- Overall width
  return $ maxB nodeW childW

nodeHeight :: (JSCanvas, JSString, JSArray JSMathE) -> JSA JSNumber
nodeHeight (c, node, children) = do
  -- Height of the node text
  nodeH <- nodeBoxHeight (c, node)
  -- Height of the children
  let foldFun e h = do
        let (widthF, heightF, renderF, _) = match e
        eHeight <- heightF $$ c
        return $ maxB eHeight h
  childH <- foldArray foldFun 0 children
  -- Overall height
  return $ nodeH + childH

drawNode :: (JSCanvas, JSString, JSArray JSMathE) -> JSA ()
drawNode (c, node, children) = do
  -- Render node text
  nodeBoxW <- nodeBoxWidth (c, node)
  nodeW    <- nodeWidth (c, node, children)
  nodeBoxH <- nodeBoxHeight (c, node)
  childW   <- childWidth (c, children)
  let nodeLocX = nodeW / 2 - nodeBoxW / 2
  c # save
  c # translate (nodeLocX, 0)
  drawNodeBox (c, node)
  c # restore
  -- Render children
  let offset = ifB (childW <* nodeW) (nodeW / 2 - childW / 2) (0)
  let foldFun e offset = do
        let (widthF, _, renderF, _) = match e
        c # save
        childW <- widthF $$ c
        c # drawLink (nodeLocX + nodeBoxW / 2, nodeBoxH - margin) 
                     (offset + childW / 2, nodeBoxH + margin)
        c # translate (offset, nodeBoxH)
        renderF $$ c
        --c # renderOutline e -- DEBUG
        c # restore
        return $ childW + offset
  _ <- foldArray foldFun offset children
  return ()

foldArray :: (SunroofArgument a, Sunroof a, Sunroof b) 
          => (a -> b -> JSA b) -> b -> JSArray a -> JSA b
foldArray f e l = do
  jsF <- function (uncurry f)
  accum <- newJSRef e
  l # forEach $ \x -> modifyJSRef (\v -> jsF $$ (x, v)) accum
  readJSRef accum



















