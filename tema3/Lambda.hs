module Lambda where

import Data.List
import Expr

-- TODO 1.1. find free variables of a Expr

free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x expr) = filter (/= x) (free_vars expr)
free_vars (Application e1 e2) = nub (free_vars e1 ++ free_vars e2)

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable x) var expr = if x == var then expr else Variable x
reduce (Function x func) var expr
  | x == var = Function x func
  | x `elem` free_vars expr = Function x2 (reduce (reduce func x (Variable x2)) var expr)
  | otherwise = Function x (reduce func var expr)
  where
     x2 = createNewVarName (free_vars func ++ free_vars expr) x
reduce (Application e1 e2) var expr = Application (reduce e1 var expr) (reduce e2 var expr)

-- functie care imi creaza un nou nume pentru o variabila
createNewVarName :: [String] -> String -> String
createNewVarName usedVars x
  | x `elem` usedVars = createNewVarName usedVars ( x ++ "#")
  | otherwise = x
  
-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable x) = Variable x
stepN (Function x func) = Function x (stepN func)
stepN (Application (Function x func) arg) = reduce func x arg
stepN (Application e1 e2)
  | isApplication e1 = Application (stepN e1) e2 
  | otherwise = Application e1 (stepN e2)

-- functie care verifica daca epresia este de tip Aplication
isApplication :: Expr -> Bool
isApplication (Application _ _) = True
isApplication _ = False


-- TODO 1.4. perform Normal Evaluation

reduceN :: Expr -> Expr
reduceN expr = case stepN expr of
  expr2 -> if expr2 == expr then expr2 else reduceN expr2


reduceAllN :: Expr -> [Expr]
reduceAllN expr = expr : case stepN expr of
  expr2 -> if expr2 == expr then [] else reduceAllN expr2

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable x) = Variable x
stepA (Function x func) = Function x (stepA func)
stepA (Application (Function x func) arg)
  | isApplication arg = Application (Function x func) (stepA arg)
  | otherwise = reduce func x arg
stepA (Application e1 e2)
  | isApplication e1 = Application (stepA e1) e2 
  | otherwise = Application e1 (stepA e2)



-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr = case stepA expr of
  expr2 -> if expr2 == expr then expr2 else reduceA expr2


reduceAllA :: Expr -> [Expr]
reduceAllA expr = expr : case stepA expr of
  expr2 -> if expr2 == expr then [] else reduceAllA expr2



-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros ctx expr = case expr of
  Variable x -> expr
  Function x e0 -> Function x (evalMacros ctx e0)
  Application e1 e2 -> Application (evalMacros ctx e1) (evalMacros ctx e2)
  Macro name -> case lookup name ctx of
    Just macroExpr -> evalMacros ctx macroExpr
    Nothing -> expr


-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy codes = evalCodeHelper [] strategy codes

-- helper pentru functia evalCode
evalCodeHelper :: [(String, Expr)] -> (Expr -> Expr) -> [Code] -> [Expr]
evalCodeHelper _ _ [] = []
evalCodeHelper macros strategy (code:codes) = case code of
  Evaluate expr -> result : evalCodeHelper macros strategy codes
    where
      result = strategy (evalMacros macros expr)
  Assign name expr -> evalCodeHelper ((name, expr) : macros) strategy codes
