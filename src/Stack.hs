{-# LANGUAGE FlexibleInstances #-}

module Stack where

-- import Control.Monad (forM_)
import Data.List (intercalate)
import Text.Printf

data Op = Mult
        | Div
        | Plus
        | Minus
        | Pow
        | Sin
        | Cos
        | Sqrt

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show Pow = "^"
  show Sin = "sin"
  show Cos = "cos"
  show Sqrt = "sqrt"

data Atom = Number Float | Operator Op

showStack :: Stack -> String
showStack s =
    let numbered = (reverse $ numberify s)
        stringed = map (\(n,a') -> show n ++ ":\t" ++ show a') numbered
    in intercalate "\n" stringed ++ "\n"

instance Show Atom where
  show (Number f) = show f
  show (Operator o) = show o

instance Num Atom where
  (+) (Number a) (Number b) = Number (a+b)
  (-) (Number a) (Number b) = Number (a-b)
  (*) (Number a) (Number b) = Number (a*b)
  negate (Number a) = Number (-a)
  abs (Number a) = Number (abs a)
  signum (Number a) = Number (signum a)
  fromInteger a = Number (fromInteger a)

instance Fractional Atom where
  (/) (Number a) (Number b) = Number (a/b)

instance Floating Atom where
  (**) (Number a) (Number b) = Number (a**b)
  sin (Number a) = Number (sin $ degToRad a)
  cos (Number a) = Number (cos $ degToRad a)
  sqrt (Number a) = Number $ sqrt a

degToRad :: Floating a => a -> a
degToRad a = 2*pi*a / 360

type Stack = [Atom]

-- modifies stack by applying stack's head operator to values
-- current evalStack assumes all operators take 2 arguments
-- new one will figure it out based on which operator is used
evalStack :: Stack -> Stack
evalStack s = let n = numArgs (head s) + 1
                  newval = apply $ take n s
              in newval : drop n s

numArgs :: Atom -> Int
numArgs (Operator op) = case op of
  Plus -> 2
  Minus -> 2
  Mult -> 2
  Div -> 2
  Pow -> 2
  Sin -> 1
  Cos -> 1
  Sqrt -> 1

apply :: Stack -> Atom
apply (op:xs) = let (Operator o) = op
                in case o of
                    Plus -> xs !! 1 + head xs
                    Minus -> xs !! 1 - head xs
                    Mult -> xs !! 1 * head xs
                    Div -> xs !! 1 / head xs
                    Pow -> xs !! 1 ** head xs
                    Sin -> sin $ head xs
                    Cos -> cos $ head xs
                    Sqrt -> sqrt $ head xs

isOperator :: Atom -> Bool
isOperator (Operator _) = True
isOperator _ = False

-- adds new value to stack, if number
-- otherwise eval stack with operator added,
-- adding the result to the top of the stack instead
push :: Stack -> Atom -> Stack
push s a =
  if isOperator a
     then evalStack (a:s)
     else a:s

readNumber :: Atom -> Float
readNumber (Number n) = n

singleton :: [a] -> Bool
singleton (_:[]) = True
singleton _ = False

reduceStack :: Stack -> Stack
reduceStack = until singleton reduce

reduce :: Stack -> Stack
reduce s = let (n, a) = findOperator s
               args = numArgs a
               comp = take (args+1) $ drop (n-args) s
               before = take (n-args) s
               after = drop (n+1) s
           in before ++ [apply (reverse comp)] ++ after


-- returns the index of the stack and which operator it is
findOperator :: Stack -> (Int, Atom)
findOperator = go 0
  where go n s = if isOperator (s !! n)
                    then (n, s !! n)
                    else go (n+1) s

numberify :: [a] -> [(Int, a)]
numberify = zip [1..]

printStack :: Stack -> IO ()
printStack = printf . showStack
