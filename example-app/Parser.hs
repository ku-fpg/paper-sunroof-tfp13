
module Parser
  ( parseMathE
  ) where

import Data.Attoparsec.Char8 
  ( Parser, parseOnly
  , takeWhile1, satisfy
  , skipSpace, double )
import Data.Char ( isLetter )
import qualified Data.ByteString.Char8 as BS

import Control.Applicative hiding ( empty )

import Types

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