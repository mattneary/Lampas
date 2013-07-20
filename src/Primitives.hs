module Primitives where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import IO
import Types
import Interfaces
import Refs
import Eval

-- ## Primitive Operations
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("=", numBoolBinop (==)),
			  ("<", numBoolBinop (<)),
  			  (">", numBoolBinop (>)),
			  ("/=", numBoolBinop (/=)),
			  (">=", numBoolBinop (>=)),
			  ("<=", numBoolBinop (<=)),
			  ("&&", boolBoolBinop (&&)),
			  ("_or", boolBoolBinop (||)),
			  ("string=?", strBoolBinop (==)),
			  ("string<?", strBoolBinop (<)),
			  ("string>?", strBoolBinop (>)),
			  ("string<=?", strBoolBinop (<=)),
			  ("string>=?", strBoolBinop (>=)),
			  ("string->list", toCharList),
			  ("list->string", fromCharList),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("atom?", atom)]             

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("eval", evil),
                ("evalenv", evilenv)]          
           
-- ## Expose a User-Land Eval Function
evil :: [LispVal] -> IOThrowsError LispVal
evil [exprs] = do
    env <- liftIO $ primitiveBindings >>= flip bindVars [] 
    eval env exprs
    
evilenv :: [LispVal] -> IOThrowsError LispVal
evilenv [List defs, exprs] = do
    env <- liftIO $ nullEnv >>= flip bindVars (map (\(List [String key, val]) -> (key, val)) defs)
    eval env exprs
                
-- ## Bind Primitives to Environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)                   
                
-- ## Elementary Functions
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList   

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList                

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList  

atom :: [LispVal] -> ThrowsError LispVal
atom [Atom val] = return $ Bool True
atom [Number val] = return $ Bool True
atom [String val] = return $ Bool True
atom other = return $ Bool False

-- ## Data-Type Conversions
-- ### string->list
toStrings :: String -> [LispVal]
toStrings [] = []
toStrings (x:xs) = (String [x]) : (toStrings xs)

toCharList :: [LispVal] -> ThrowsError LispVal
toCharList args = do str <- unpackStr $ args !! 0
                     return $ List $ toStrings str
                     
-- *used internally to allow for recursion (avoid monad mess)*
fromCharList' :: [LispVal] -> LispVal
fromCharList' [List (String first : rest)] = String (first ++ ((unpackStr' . fromCharList') rest))
fromCharList' ((String first) : rest) = String (first ++ ((unpackStr' . fromCharList') rest))
fromCharList' [] = String ""
fromCharList' notList = String ""

-- ### list->string
fromCharList :: [LispVal] -> ThrowsError LispVal
fromCharList x = return $ fromCharList' x 