
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Types ( JSMathE, JSMessage ) where

import Data.Boolean

import Language.Sunroof
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Canvas ( JSCanvas )
 
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
                           , JSMessage -- Result Message
                           )
  match o = ( o ! "width", o ! "height", o ! "render", o ! "result" )
  tuple (w, h, render, res) = do
    o <- new "Object" ()
    o # "width"  := w
    o # "height" := h
    o # "render" := render
    o # "result" := res
    return $ JSMathE o

-- JSMessage ---------------------------------------------------

newtype JSMessage = JSMessage JSObject

instance Show JSMessage where
  show (JSMessage o) = show o

instance Sunroof JSMessage where
  box = JSMessage . box
  unbox (JSMessage o) = unbox o

type instance BooleanOf JSMessage = JSBool

instance IfB JSMessage where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSMessage where
  (JSMessage a) ==* (JSMessage b) = a ==* b

instance JSTuple JSMessage where
  type Internals JSMessage = ( JSString -- Message Type
                             , JSObject -- Message Content
                             )
  match o = ( o ! "type", o ! "content" )
  tuple (t, c) = do
    o <- new "Object" ()
    o # "type"    := t
    o # "content" := c
    return $ JSMessage o