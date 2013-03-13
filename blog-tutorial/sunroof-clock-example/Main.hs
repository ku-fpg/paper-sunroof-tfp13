{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds #-}

module Main where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers
import Data.Default

import Control.Monad
import Control.Monad.IO.Class

import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser hiding ( eval )
import Language.Sunroof.JS.JQuery

main :: IO ()
main = staticCompiler def "main" clockJS >>= writeFile "main.js"

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

-- TODO: Would be neat to create JS functions with more then one parameter.
clockJS :: JS A ()
clockJS = do

  -- Renders a single line (with number) of the clock face.
  renderClockFaceLine <- function $ \(c, u, n) -> do
    c # save
    -- Draw one of the indicator lines
    c # beginPath
    c # moveTo (0, -u * 1.0)
    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (c # lineTo (0, -u * 0.8)) -- Minute line
        (c # lineTo (0, -u * 0.9)) -- Hour line
    ifB (n `mod` 15 ==* (0 :: JSNumber))
        (c # setLineWidth 8) -- Quarter line
        (c # setLineWidth 3) -- Non-Quarter line
    c # stroke
    c # closePath
    -- Draw of the hour numbers
    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (do
          c # translate (-u * 0.75, 0)
          c # rotate (-2 * pi / 4)
          c # fillText (cast $ n `div` 5) (0, 0)
        ) (return ())
    c # restore

  -- Renders a single clock pointer.
  renderClockPointer <- function $ \(c, u, angle, width, len) -> do
    c # save
    c # setLineCap "round"
    c # rotate angle
    c # setLineWidth width
    c # beginPath
    c # moveTo (0, u * 0.1)
    c # lineTo (0, -u * len)
    c # stroke
    c # closePath
    c # restore
  -- Renders the clocks pointers for hours, minutes and seconds.
  renderClockPointers <- function $ \(c, u) -> do
    (h, m, s) <- currentTime
    c # save
    c # setLineCap "round"
    -- Hour pointer
    apply renderClockPointer
      (c, u, (2 * pi / 12) * ((h `mod` 12) + (m `mod` 60) / 60), 15, 0.4)
    -- Minute pointer
    apply renderClockPointer
      ( c, u, (2 * pi / 60) * ((m `mod` 60) + (s `mod` 60) / 60), 10, 0.7)
    -- Second pointer
    c # setStrokeStyle "red"
    apply renderClockPointer
      ( c, u, (2 * pi / 60) * (s `mod` 60), 4, 0.9)
    -- Restore everything
    c # restore

  -- Renders the complete face of the clock, without pointers.
  renderClockFace <- function $ \(c, u) -> do
    c # save
    c # rotate (2 * pi / 4) -- 0 degrees is at the top
    -- Draw all hour lines.
    array [1..60::Int] # forEach $ \n -> do
      c # save
      c # rotate ((2 * pi / 60) * n)
      renderClockFaceLine `apply` (c, u, n)
      c # restore
    c # restore -- Undo all the rotation.

  -- Renders the complete clock.
  renderClock <- function $ \() -> do
    u <- clockUnit
    (w,h) <- canvasSize
    c <- context
    -- Basic setup
    c # save
    c # setFillStyle "black"
    c # setStrokeStyle "black"
    c # setLineCap "round"
    c # setTextAlign "center"
    c # setFont ((cast $ u * 0.1) <> "px serif")
    c # setTextBaseline "top"
    c # clearRect (0,0) (w,h)
    c # translate (w / 2, h / 2)
    -- Draw all hour lines.
    renderClockFace `apply` (c, u)
    -- Draw the clock pointers
    renderClockPointers `apply` (c, u)
    c # restore
    return ()

  renderClock `apply` ()
  window # setInterval renderClock 1000

  return ()

canvas :: JS A JSObject
canvas = document # getElementById "canvas"

context :: JS A JSObject
context = canvas >>= getContext "2d"

clockUnit :: JS A JSNumber
clockUnit = do
  (w, h) <- canvasSize
  return $ (maxB w h) / 2

canvasSize :: JS A (JSNumber, JSNumber)
canvasSize = do
  c <- jQuery "#canvas"
  w <- c # method "innerWidth" ()
  h <- c # method "innerHeight" ()
  return (w, h)

currentTime :: JS A (JSNumber, JSNumber, JSNumber)
currentTime = do
  date <- evaluate $ object "new Date()"
  h <- date # method "getHours" ()
  m <- date # method "getMinutes" ()
  s <- date # method "getSeconds" ()
  return (h, m, s)
