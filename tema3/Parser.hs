module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr

import Data.Char


-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
  mp >>= f =
    Parser $ \s -> case parse mp s of
                      Nothing -> Nothing
                      Just(x,s') -> parse (f x) s' 
  return :: a -> Parser a
  return x = Parser $ \s -> Just(x,s)

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s
                                ok -> ok

--- type declaration over ---

failParser :: Parser a
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of
                                [] -> Nothing
                                (x:xs) -> if x == c then Just (x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser testing = Parser $
    \s -> case s of
        [] -> Nothing
        (x:xs) -> if testing x then Just (x, xs) else Nothing

skipSpaces :: Parser ()
skipSpaces = do
    _ <- many (predicateParser isSpace)
    return ()

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr string = case parse expressionParser ("(" ++ string ++ ")") of
    Just (expr, _) -> expr
    Nothing -> error string

expressionParser :: Parser Expr
expressionParser = variableParser <|> functionParser <|> applicationParser <|> macroParser

variableParser :: Parser Expr
variableParser = do
    x <- predicateParser isAlphaNum
    return (Variable [x])

functionParser :: Parser Expr
functionParser = do
    _ <- charParser '\\'
    x <- predicateParser isAlphaNum
    _ <- charParser '.'
    expr <- expressionParser
    return (Function [x] expr)


applicationParser :: Parser Expr
applicationParser = do
    _ <- charParser '('
    expr <- expressionParser
    exprs <- many (skipSpaces >> expressionParser)
    _ <- charParser ')'
    return (foldl Application expr exprs)


macroParser :: Parser Expr
macroParser = do
    _ <- charParser '$'
    name <- many (predicateParser isAlphaNum)
    return (Macro name)


-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code string = case parse codeParser string of
    Just (code, _) -> code
    Nothing -> error string

codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser

assignParser :: Parser Code
assignParser = do
    name <- many (predicateParser isAlphaNum)
    skipSpaces
    _ <- charParser '='
    skipSpaces
    expr <- expressionParser
    return (Assign name expr)

evaluateParser :: Parser Code
evaluateParser = do
  expr <- expressionParser
  exprs <- many (skipSpaces >> expressionParser)
  return (Evaluate (foldl Application expr exprs))
