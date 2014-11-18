module Stack where

import Data.IORef
import Control.Monad (forM_)

data InteractFlag = Interactive | NoInteractive

data Op = Mult
        | Div
        | Plus
        | Minus
        | Pow

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show Pow = "^"

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

type Stack = [Atom]

-- modifies stack by applying stack's head operator to values
evalStack :: Stack -> Stack
evalStack s = let (Operator o) = head s
                  x = s !! 2
                  y = s !! 1
              in case o of
                  Plus  -> x + y  : drop 3 s
                  Minus -> x - y  : drop 3 s
                  Mult  -> x * y  : drop 3 s
                  Div   -> x / y  : drop 3 s
                  Pow   -> x ** y : drop 3 s

-- adds new value to stack, if number
push :: IORef Stack -> InteractFlag -> Atom -> IO ()
push s Interactive a@(Number n) = do
  modifyIORef s (a:)
  print n
push s NoInteractive a@(Number n) = modifyIORef s (a:)
push s Interactive a@(Operator _) = do
  modifyIORef s (a:)
  modifyIORef s evalStack
  peak s >>= print
push s NoInteractive a@(Operator _) = do
  modifyIORef s (a:)
  modifyIORef s evalStack

peak :: IORef Stack -> IO Atom
peak s = do
  stack <- readIORef s
  return $ head stack

tokenize :: Floating a => Atom -> a -> a -> a
tokenize (Operator Plus)  = (+)
tokenize (Operator Minus) = (-)
tokenize (Operator Mult)  = (*)
tokenize (Operator Div)   = (/)
tokenize (Operator Pow)   = (**)
tokenize (Number _)       = error "Cannot tokenize a number"

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough p xs = takeWhile p xs ++ [head $ dropWhile p xs]

computeOperation :: Stack -> Atom
computeOperation (x:y:op:[]) = tokenize op x y

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

readNumber :: Atom -> Float
readNumber (Number n) = n

reduceStack :: Stack -> IO Float
reduceStack stack = do
  ref <- newIORef ([] :: Stack)
  forM_ stack $ \a -> push ref NoInteractive a
  res <- readIORef ref
  (return . readNumber . head) res
