module Eval where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import Types
import Refs
import IO

-- ## Apply Arguments to Functions
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
-- ### Apply Arguments to User-Defined Functions
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 
apply (IOFunc func) args = func args  

-- ## Apply a List of Arguments to Functions
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args            

-- ## Macros

-- ### Check for Rewriters of Expressions
hasRewrite :: LispVal -> Env -> IOThrowsError Bool
hasRewrite (Atom name) env = liftIO $ isBound env (name ++ "-syntax")
hasRewrite badAtom env = liftIO $ return False

hasEnvRewrite :: LispVal -> Env -> IOThrowsError Bool
hasEnvRewrite (Atom name) env = liftIO $ isBound env (name ++ "-syntax-env")
hasEnvRewrite badAtom env = liftIO $ return False

-- ### Macro Rewriters
rewrite env (Atom name) args = do
    func <- getVar env (name ++ "-syntax")    
    applied <- apply func args
    eval env applied
    
rewriteEnv env (Atom name) args = do
    func <- getVar env (name ++ "-syntax-env")    
    renderedEnv <- renderEnv env
    renderedVals <- mapM (\(String val) -> getVar env val) renderedEnv
    applied <- apply func ((List (map (\(key, val) -> List [key, val]) (zip renderedEnv renderedVals))):args)
    eval env applied    
    
-- ## Quasiquotations
evalCommas (List [Atom "unquote", val]) = val
evalCommas normalAtom = List [Atom "quasiquote", normalAtom]

-- ## Evaluate LispVals
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", List args]) = do 
    argVals <- mapM ((eval env) . evalCommas) args
    liftIO $ return $ List argVals
eval env (List [Atom "quasiquote", arg]) = liftIO $ return arg
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)    
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "defenvmacro" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env (var ++ "-syntax-env")    
eval env (List (Atom "defmacro" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env (var ++ "-syntax")        
eval env (List (Atom "defmacro" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env (var ++ "-syntax")    
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body    
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env val@(List (function : args)) = do
    hadRewrite <- hasRewrite function env
    hadEnvRewrite <- hasEnvRewrite function env
    if hadRewrite 
      then rewrite env function args 
      else if hadEnvRewrite
        then rewriteEnv env function args
        else evalfun env val
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalfun env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

-- ## Function Constructors
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal