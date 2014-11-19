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

mathFun :: Parser Atom
mathFun = try sinP <|> try cosP <|> sqrtP

constant :: Parser Atom
constant = do
  string "pi"
  return $ Number 3.145926

cosP :: Parser Atom
cosP = do
  string "cos"
  return $ Operator Cos

sqrtP :: Parser Atom
sqrtP = do
  string "sqrt"
  return $ Operator Sqrt

sinP :: Parser Atom
sinP = do
  string "sin"
  return $ Operator Sin

atom :: Parser Atom
atom = number <|> operator <|> mathFun <|> constant
