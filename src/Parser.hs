module Parser where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import Types

-- ## Symbol Class of Characters
symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?^_~@"

-- ## Parsing
-- The main parser, `parseExpr` attempts parsing of the various expression types.
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseVector
         <|> parseLambda  
         <|> parseUnQuoted         
         <|> parseQuasiQuoted       
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- ## Parse

-- Parses atomic values as `#t`, `#f` or a regulat atom.
parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of 
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom   

-- Parses quoted strings, currently without escaping of quotes.
parseString :: Parser LispVal
parseString = do
                  char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x                 
                                
-- Parses basic integers.                                                   
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit                                          
         
-- Reads in a basic S-Expression         
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- Parses dotted lists, i.e., `(1 (2 3)) = (1 . 2 . 3)`
parseDottedList :: Parser LispVal
parseDottedList = do
     head <- endBy parseExpr space
     tail <- char '.' >> spaces >> parseExpr
     return $ DottedList head tail

-- Parses quoted expressions.
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]       
    
-- Parses the vector shorthand, `[...]` as a normal list. In the future these will be generators.
parseVector :: Parser LispVal
parseVector = do 
       char '['
       x <- try parseList <|> parseDottedList
       char ']'
       return $ List [Atom "quote", x]
       
-- Parses quasiquoted expressions.       
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]     
    
-- Parses unquoting, currently even outside of quasiquotes.    
parseUnQuoted :: Parser LispVal
parseUnQuoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]                       
       
-- Parses the lambda shorthand, `{|x| ...}`.
parseLambda :: Parser LispVal
parseLambda = do 
      string "{|"      
      args <- try parseList <|> parseDottedList
      string "| "
      body <- try (sepBy parseExpr spaces)
      optional (char ' ')
      char '}'
      return $ List ((Atom "lambda"):args:body)