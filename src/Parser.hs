module Parser where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

import Stack

number :: Parser String
number = many1 digit

minus :: Parser String
minus = (:) <$> char '-' <*> number

integer :: Parser String
integer = {- plus <|>  -} minus <|> number

float :: Parser Float
float = fmap rd $ (++) <$> integer <*> decimal
  where rd = read :: String -> Float
        decimal = option "" $ (:) <$> char '.' <*> number

atomNumber :: Parser Atom
atomNumber = Number <$> float

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

piP :: Parser Atom
piP = do
  _ <- string "pi"
  return $ Number 3.145926

eP :: Parser Atom
eP = do
  _ <- string "e"
  return $ Number 2.7181818

constants :: Parser Atom
constants = piP <|> eP


cosP :: Parser Atom
cosP = do
  _ <- string "cos"
  return $ Operator Cos

sqrtP :: Parser Atom
sqrtP = do
  _ <- string "sqrt"
  return $ Operator Sqrt

sinP :: Parser Atom
sinP = do
  _ <- string "sin"
  return $ Operator Sin

atom :: Parser Atom
atom = try atomNumber <|> try operator <|> try mathFun <|> constants
