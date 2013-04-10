
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Data.Monoid
import Data.Boolean
import Language.Sunroof
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.Canvas

test :: (Sunroof a) => a -> IO ()
test o = sunroofCompileJS def "main" (return o) >>= putStrLn

expr1 :: JSBool
expr1 = ifB (true &&* false) (notB false) (false ||* true) :: JSBool

expr2 :: JSNumber
expr2 = (3 + 4) * 5 :: JSNumber

expr3 :: JSNumber -> JSNumber -> JSBool
expr3 x y = (x - 5 >=* -10 &&* x * 2 <=* 10) &&* (x /=* 0 ||* notB (y + 3 ==* x))

expr4 :: JSString -> JSString
expr4 name = "Your name is " <> name

expr5 :: JSArray JSNumber
expr5 = array [0 .. 5 :: Int]

expr6 :: (Sunroof a) => JSArray a -> a
expr6 arr = arr ! index 0

expr7 :: (Sunroof a) => JSObject -> a
expr7 obj = obj ! label "name"

testJS :: JSA () -> IO ()
testJS o = sunroofCompileJS def "main" o >>= putStrLn

askName :: JSA ()
askName = do
      name <- prompt "What is your name?" ""
      alert $ "Your name is " <> cast name <> "!"

drawBox :: JSA ()
drawBox = do
      canvas <- document # getElementById "canvas"
      context <- canvas # getContext "2d"
      context # fillRect (10, 10) (100, 100) 








