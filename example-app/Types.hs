
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Types 
  ( MathE(..)
  , evalM
  , ErrorE
  , errorE
  , JSTree, JSTreeTuple(..)
  , jsTree
  , JSMessage, JSMessageTuple(..)
  , jsMessage, jsResult
  ) where

import Data.Boolean

import Language.Sunroof
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Canvas ( JSCanvas )

-- -------------------------------------------------------------

type ErrorE a = Either String a

errorE :: String -> ErrorE a
errorE = Left

-- MathE -------------------------------------------------------

data MathE = NumE Double
           | OpE MathE Char MathE
           | FunE String MathE
           deriving Show

-- Evaluation --------------------------------------------------

opM :: Char -> ErrorE (Double -> Double -> ErrorE Double)
opM o = case o of
  '+' -> return $ \d1 d2 -> return $ d1 + d2
  '-' -> return $ \d1 d2 -> return $ d1 - d2
  '*' -> return $ \d1 d2 -> return $ d1 * d2
  '/' -> return $ \d1 d2 -> if d2 == 0 
                               then errorE "Division by zero!" 
                               else return $ d1 / d2
  op  -> errorE $ "Unknown operator: " ++ [op]

funM :: String -> ErrorE (Double -> ErrorE Double)
funM f = case f of
  "log" -> return $ \d -> if d < 0 
                             then errorE "Logarithm of a negative number!"
                             else return $ log d
  "cos" -> return $ return . cos
  "sin" -> return $ return . sin
  "tan" -> return $ \d -> if isNaN (tan d)
                             then errorE $ "tan undefined for " ++ show d ++ "!"
                             else return $ tan d
  fun -> return $ \_ -> errorE $ "Undefined function '" ++ fun ++ "'!"

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

-- JSMathE -----------------------------------------------------

newtype JSTree = JSTree JSObject

data JSTreeTuple = JSTreeTuple 
  { treeNode     :: JSString
  , treeChildren :: JSArray JSTree
  , treeResult   :: JSMessage
  , treeSelected :: JSBool }

instance Show JSTree where
  show (JSTree o) = show o

instance Sunroof JSTree where
  box = JSTree . box
  unbox (JSTree o) = unbox o

type instance BooleanOf JSTree = JSBool

instance IfB JSTree where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSTree where
  (JSTree a) ==* (JSTree b) = a ==* b

instance JSTuple JSTree where
  type Internals JSTree = JSTreeTuple
  match o = JSTreeTuple 
    { treeNode = o ! "text"
    , treeChildren = o ! "children"
    , treeResult = o ! "result"
    , treeSelected = o ! "selected" }
  tuple t = do
    o <- new "Object" ()
    o # "text"  := treeNode t
    o # "children" := treeChildren t
    o # "result" := treeResult t
    o # "selected" := treeSelected t
    return $ JSTree o

jsTree :: JSString -> JSArray JSTree -> JSMessage -> JSA JSTree
jsTree node children result = 
  tuple $ JSTreeTuple { treeNode = node
                      , treeChildren = children
                      , treeResult = result
                      , treeSelected = false }

-- JSMessage ---------------------------------------------------

newtype JSMessage = JSMessage JSObject

data JSMessageTuple = JSMessageTuple 
  { msgType :: JSString
  , msgContent :: JSObject }

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
  type Internals JSMessage = JSMessageTuple
  match o = JSMessageTuple 
    { msgType = o ! "type"
    , msgContent = o ! "content" }
  tuple m = do
    o <- new "Object" ()
    o # "type"    := msgType m
    o # "content" := msgContent m
    return $ JSMessage o

jsMessage :: (Sunroof a) => JSString -> a -> JSA JSMessage
jsMessage type' content = 
  tuple $ JSMessageTuple { msgType = type'
                         , msgContent = cast $ content }

jsResult :: ErrorE Double -> JSA JSMessage
jsResult (Left err) = jsMessage "error" (js err)
jsResult (Right v)  = jsMessage "result" (js v)