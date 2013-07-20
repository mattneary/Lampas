module Refs where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import Types

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- ## Get variable
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)
                               
-- ## Render Environment 
semiGround :: IORef LispVal -> IO LispVal
semiGround val = do
    grounded <- readIORef val
    return grounded    
                              
renderEnv :: Env -> IOThrowsError [LispVal]
renderEnv envRef = do
    env <- liftIO $ readIORef envRef
    val <- getVar envRef "list"
    let (keys, vals) = unzip env in return $ (map (\k -> String k) keys)

-- ## Set variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

-- ## Define variable
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

-- ## Bind a series of variables
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)  