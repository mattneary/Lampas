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
-- Gets all environment definitions and selects the key elements as a `LispVal`.  
renderEnv :: Env -> IOThrowsError [LispVal]
renderEnv envRef = do
    env <- liftIO $ readIORef envRef
    let (keys, vals) = unzip env in return $ (map (\k -> String k) keys)

-- ## Set Variable
-- Performs a lookup on the environment associations to find a variable value.
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

-- ## Define variable
-- Either finds and modifies an association on the `env` or creates a new one.
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
-- Converts a series of associations to an environment. Useful in initiation and `evalenv`.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)  