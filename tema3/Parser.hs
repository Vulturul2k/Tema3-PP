{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
  --(>>=) :: m a -> (a -> m b) -> m b
  mp >>= f =
    Parser $ \s -> case parse mp s of
                      Nothing -> Nothing
                      Just(x,s') -> parse (f x) s' 
  --return :: a -> m a
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

failParser :: Parser a
failParser = Parser $ \s -> Nothing

--- type declaration over ---

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr s = case parse exprParser ("(" ++ s ++ ")") of
    Just (expr, _) -> expr
    Nothing -> error s

exprParser :: Parser Expr
exprParser = variableParser <|> functionParser <|> applicationParser <|> macroParser

variableParser :: Parser Expr
variableParser = do
    x <- predicateParser isAlpha
    return (Variable [x])

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing

functionParser :: Parser Expr
functionParser = do
    _ <- charParser '\\'
    x <- predicateParser isAlpha
    _ <- charParser '.'
    e <- exprParser
    return (Function [x] e)

applicationParser :: Parser Expr
applicationParser = do
    _ <- charParser '('
    e1 <- exprParser
    es <- many exprParserFunc
    _ <- charParser ')'
    return (buildApplication e1 es)

buildApplication :: Expr -> [Expr] -> Expr
buildApplication e [] = e
buildApplication e (e' : es) = buildApplication (Application e e') es

exprParserFunc :: Parser Expr
exprParserFunc = do
    skipSpaces
    exprParser

skipSpaces :: Parser ()
skipSpaces = do
    _ <- many (charParser ' ')
    return ()


charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of
                                [] -> Nothing
                                (x:xs) -> if x == c then Just (x, xs) else Nothing

macroParser :: Parser Expr
macroParser = do
    _ <- charParser '$'
    name <- identifierParser
    return (Macro name)

identifierParser :: Parser String
identifierParser = do
    x <- predicateParser isAlpha
    xs <- many (predicateParser isAlphaNum)
    return (x:xs)

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code s = case parse codeParser s of
    Just (code, _) -> code
    Nothing -> error s

codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser

evaluateParser :: Parser Code
evaluateParser = do
  skipSpaces
  expr <- exprParser
  exprs <- many (skipSpaces >> exprParser)
  let exprList = expr : exprs
  return (Evaluate (foldl1 Application exprList))

assignParser :: Parser Code
assignParser = do
    name <- identifierParser
    skipSpaces
    _ <- charParser '='
    skipSpaces
    expr <- exprParser
    return (Assign name expr)
