module Stack where

import Data.IORef
import Control.Monad (when, forM_)

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
-- otherwise eval stack with operator added,
-- adding the result to the top of the stack instead
push :: IORef Stack -> Atom -> IO ()
push s a@(Number n) = do
  modifyIORef s (a:)
push s a@(Operator _) = do
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

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

readNumber :: Atom -> Float
readNumber (Number n) = n

-- ERROR: always displays head of stack as answer
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
