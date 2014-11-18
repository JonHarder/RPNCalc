module Parser where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))

import Stack

number :: Parser Atom
number = (Number . read) <$> many1 (digit <|> char '.')

operator :: Parser Atom
operator = do
  c <- oneOf "+-*^/"
  return $ Operator $ case c of
      '+' -> Plus
      '-' -> Minus
      '*' -> Mult
      '/' -> Div
      '^' -> Pow
      _   -> error $ "unrecognized opperator: " ++ [c]

atom :: Parser Atom
atom = number <|> operator
