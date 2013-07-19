module Parser where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

import Types

-- | # Symbol Class of Characters
symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?@^_~"

-- | # Parsing
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

-- | # Parse Atom/String/Number/List/Quoted/Vector/Lambda
parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of 
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom   

parseString :: Parser LispVal
parseString = do
                  char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x                 
                                                   
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit                                          
         
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
     head <- endBy parseExpr space
     tail <- char '.' >> spaces >> parseExpr
     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]       
    
parseVector :: Parser LispVal
parseVector = do 
       char '['
       x <- try parseList <|> parseDottedList
       char ']'
       return $ List [Atom "quote", x]
       
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]     
    
parseUnQuoted :: Parser LispVal
parseUnQuoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]             
       
parseLambda :: Parser LispVal
parseLambda = do 
      string "{|"      
      args <- try parseList <|> parseDottedList
      string "| "
      body <- try (sepBy parseExpr spaces)
      optional (char ' ')
      char '}'
      return $ List ((Atom "lambda"):args:body)