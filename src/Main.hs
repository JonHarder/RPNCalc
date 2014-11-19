import Text.ParserCombinators.Parsec
import Data.IORef
import Control.Monad (when, unless)
import System.IO
import System.Environment

import Parser
import Stack

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

promptLine :: String -> IO String
promptLine prompt = flushStr prompt >> getLine

parseAtom :: String -> Atom
parseAtom str = case parse atom "Parsing Calculation" str of
  Left e -> error $ show e
  Right a -> a


main :: IO ()
main = do
  args <- getArgs
  env <- newIORef ([] :: Stack)
  case length args of
   0 -> loop env
   _ -> let argStack = map parseAtom $ words $ head args
        in reduceStack argStack >>= print
 where loop stack = do
         str <- promptLine "RPN>> "
         unless ((== "quit") str) $
           if str == "clear"
              then do writeIORef stack []
                      loop stack

              else do push stack (parseAtom str)
                      printStack stack
                      loop stack
