module Types where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

-- ## Environment
-- A list of associations of values.
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue   

-- ## Data Type Interfaces
-- Definitions or packagings that constitute each data type.
instance Show LispVal where show = showVal
data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              | IOFunc ([LispVal] -> IOThrowsError LispVal)
              | Port Handle
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | Func {params :: [String], vararg :: (Maybe String), 
                                    body :: [LispVal], closure :: Env}
                                    
-- ## Type Renderers for REPL
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)" 
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"                                     
                                    
-- ## Error Handling
instance Show LispError where show = showError
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String                                    
               
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr               

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- ## Inherit from Haskell Error
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError 