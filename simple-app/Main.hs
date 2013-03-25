
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Default ( Default(..) )
import Data.Monoid  ( (<>) )
import Data.Boolean

import Language.Sunroof
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.Date
import Language.Sunroof.JS.JQuery

main :: IO ()
main = do
  src <- sunroofCompileJS def "main" $ function $ \() -> do
    k <- continuation $ \() -> jsApp
    goto k () :: JSA ()
  writeFile "main.js" src

g :: JSNumber
g = -1.0

ballRadius :: JSNumber
ballRadius = 0.1

data State = State
  { time :: JSNumber
  , posX :: JSNumber
  , posY :: JSNumber
  , velocity :: JSNumber }

newtype JSState = JSState JSObject

instance Show JSState where
  show (JSState o) = show o

instance Sunroof JSState where
  box = JSState . box
  unbox (JSState o) = unbox o

type instance BooleanOf JSState = JSBool

instance IfB JSState where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSState where
  (JSState a) ==* (JSState b) = a ==* b

instance JSTuple JSState where
  type Internals JSState = State
  match o = State 
    { time = o ! "time"
    , posX = o ! "x"
    , posY = o ! "y"
    , velocity = o ! "v" }
  tuple s = do 
    o <- new "Object" ()
    o # "time" := time s
    o # "x" := posX s
    o # "y" := posY s
    o # "v" := velocity s
    return $ cast o

jsApp :: JSB ()
jsApp = do
  startTime <- newDate() >>= getTime
  state <- tuple State { time = startTime, posX = 0.5, posY = 0.8, velocity = 0 }
  loop (state :: JSState) $ \oldState -> do
    drawBall (0.5, 0.5)
    let oldS = match oldState
    currTime <- newDate() >>= getTime
    dt <- value $ (currTime - time oldS) / 1000
    
    newV <- value $ velocity oldS + g * dt
    newX <- value $ posX oldS
    newY <- value $ posY oldS + newV * dt + (g * dt * dt / 2)
    
    drawBall (newX, newY)
    
    let newState = State 
          { time = currTime
          , posX = newX
          , posY = newY
          , velocity = newV }
    
    -- We hit the floor?
    ifB (newY <* ballRadius &&* newV <* 0)
      (tuple $ newState { velocity = -newV })
      (tuple $ newState)

drawBall :: (JSNumber, JSNumber) -> JS t ()
drawBall (x,y) = do
  canvas <- document # getElementById "canvas"
  jcanvas <- jq "#canvas"
  w <- cast `fmap` (jcanvas # attr' "width")
  h <- cast `fmap` (jcanvas # attr' "height")
  c <- canvas # getContext "2d"
  c # clearRect (0,0) (w,h)
  c # beginPath
  -- We are drawing at relative (0,1) coordinates, 
  -- in a corrected cart. coord. system.
  c # arc (x * w, h - y * h) (ballRadius * h) (0, 2*pi)
  c # closePath
  c # setFillStyle "red"
  c # fill
  return ()







