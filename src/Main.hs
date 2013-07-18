module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import Types
import Refs 
import IO  
import Eval                                                              
import Primitives                

-- | # REPL
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lampas >> ") . evalAndPrint

-- | ## REPL 1) Read
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | ## REPL 2-3) Evaluation and Printing
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- | ## REPL 4) Loop until exit action
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action
    
-- | # File Evaluator    
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
         >>= hPutStrLn stderr
     
-- | # Command-Line Interfacing
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args