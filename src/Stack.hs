module Stack where

import Data.IORef
import Control.Monad (when, forM_)

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
numArgs (Operator Plus) = 2
numArgs (Operator Minus) = 2
numArgs (Operator Mult) = 2
numArgs (Operator Div) = 2
numArgs (Operator Pow) = 2
numArgs (Operator Sin) = 1
numArgs (Operator Cos) = 1
numArgs (Operator Sqrt) = 1

apply :: Stack -> Atom
apply (op:xs) = case op of
  Operator Plus -> xs !! 1 + head xs
  Operator Minus -> xs !! 1 - head xs
  Operator Mult -> xs !! 1 * head xs
  Operator Div -> xs !! 1 / head xs
  Operator Pow -> xs !! 1 ** head xs
  Operator Sin -> sin $ head xs
  Operator Cos -> cos $ head xs
  Operator Sqrt -> sqrt $ head xs

isOperator :: Atom -> Bool
isOperator (Operator _) = True
isOperator _ = False

-- adds new value to stack, if number
-- otherwise eval stack with operator added,
-- adding the result to the top of the stack instead
push :: IORef Stack -> Atom -> IO ()
push s a = do
  modifyIORef s (a:)
  when (isOperator a) $
    modifyIORef s evalStack

readNumber :: Atom -> Float
readNumber (Number n) = n

reduceStack :: Stack -> IO Float
reduceStack stack = do
  ref <- newIORef ([] :: Stack)
  forM_ stack $ \a -> push ref a
  res <- readIORef ref
  (return . readNumber . head) res

numberify :: [a] -> [(Int, a)]
numberify = go 1
  where go _ [] = []
        go n (x:xs) = (n,x) : go (n+1) xs

printStack :: IORef Stack -> IO ()
-- printStack s = readIORef s >>= \stack -> forM_ stack $ \a -> print a
printStack s = do
  stack <- readIORef s
  forM_ (reverse $ numberify stack) $ \a -> do
    let (n, a') = a
    putStrLn $ show n ++ ":\t" ++ show a'
