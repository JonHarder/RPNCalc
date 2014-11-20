import Text.ParserCombinators.Parsec
-- import Data.IORef
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
  -- arg <- fmap (words . head) getArgs
  arg <- getArgs
  case length arg of
   0 -> loop []
   _ -> let comp = map parseAtom $ words $ head arg
        in print $ readNumber $ head $ reduceStack comp
 where loop :: Stack -> IO ()
       loop s = do
         str <- promptLine "RPN>> "
         unless (str == "quit") $
            if str == "clear"
               then loop []
               else let a = parseAtom str
                        s' = push s a
                    in printStack s' >> loop s'
