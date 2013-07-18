module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import Types
import Refs                                                                 

-- ## Parsing
-- character class `symbols`
symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?@^_~"

-- parse an atomic value either as a bool or an atom
parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of 
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom   

-- parse quoted strings
parseString :: Parser LispVal
parseString = do
                  char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x                 
                           
-- parse a string of digits as a number                           
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit                                          
         
-- parse a spaces separated list         
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces         

-- parse nestings of the `.` cons operator, i.e., nil-free lists
parseDottedList :: Parser LispVal
parseDottedList = do
     head <- endBy parseExpr space
     tail <- char '.' >> spaces >> parseExpr
     return $ DottedList head tail

-- parse the single quote syntax
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
    
-- parse vector shorthand
parseVector :: Parser LispVal
parseVector = do 
       char '['
       x <- try parseList <|> parseDottedList
       char ']'
       return $ List [Atom "quote", x]
       
-- parse ruby-style lambda shorthand
parseLambda :: Parser LispVal
parseLambda = do 
      string "{|"      
      args <- try parseList <|> parseDottedList
      string "| "
      body <- try parseExpr
      optional (char ' ')
      char '}'
      return $ List [Atom "lambda", args, body]          

-- match the expression to an expression type
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseVector
         <|> parseLambda         
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x         

-- ##Evaluation              
-- parse numbers and fold arithmetic operations
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- define boolean operations
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- parse strings
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString                                     

-- parse booleans
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- string->list
toStrings :: String -> [LispVal]
toStrings [] = []
toStrings (x:xs) = (String [x]) : (toStrings xs)

toCharList :: [LispVal] -> ThrowsError LispVal
toCharList args = do str <- unpackStr $ args !! 0
                     return $ List $ toStrings str

-- used internally
unpackStr' :: LispVal -> String
unpackStr' (String s) = s

-- used internally to allow for recursion (avoid monad mess)
fromCharList' :: [LispVal] -> LispVal
fromCharList' [List (String first : rest)] = String (first ++ ((unpackStr' . fromCharList') rest))
fromCharList' ((String first) : rest) = String (first ++ ((unpackStr' . fromCharList') rest))
fromCharList' [] = String ""
fromCharList' notList = String ""

-- list->string
fromCharList :: [LispVal] -> ThrowsError LispVal
fromCharList x = return $ fromCharList' x

-- declare primitive values
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
              ("equal?", equal)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]              
              
-- provide function constructors              
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal              

-- bind primitives to the environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func) 
    
-- define port interfaces
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode            

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- define a function applier
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
-- apply a user defined function - oh my
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

-- list application, just like Function#apply in JavaScript
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args            

-- provide pattern matching evaluation actions
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
    
-- load is a special form because it has a distinct interface which introduces bindings
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)    

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- ###elementary functions
-- car         
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList   

--cdr      
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- cons
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- eq?/eqv?
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

-- weak equivalence, i.e., == or equal?
-- forall is an existential and requires the compilation flat `-XExistentialQuantification`
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)
        
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList        

-- ##REPL/Executor
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- read in line
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- evaluation of a string
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- loop until exit action
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action
    
-- file execution     
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
         >>= hPutStrLn stderr

-- the REPL, a loop of eval-print until `quit`     
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lampas >> ") . evalAndPrint

-- ##Input
-- read in to a Lisp parser
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)
     
-- pass command line arg to the read > eval > show > render, either error or response
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args