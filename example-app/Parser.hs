
module Parser
  ( MathE(..)
  , ErrorE
  , parseMathE
  , evalM
  ) where

import Data.Attoparsec.Char8 
  ( Parser, parseOnly
  , takeWhile1, satisfy
  , skipSpace, double )
import Data.Char ( isLetter )
import qualified Data.ByteString.Char8 as BS

import Control.Applicative hiding ( empty )

-- Type --------------------------------------------------------

data MathE = NumE Double
           | OpE MathE Char MathE
           | FunE String MathE
           deriving Show

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
parseMathE str | null str = errorE "No Input"
parseMathE str = parseOnly (parseExp <* skipSpace) (BS.pack str)

-- Evaluation --------------------------------------------------

type ErrorE a = Either String a

errorE :: String -> ErrorE a
errorE = Left

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