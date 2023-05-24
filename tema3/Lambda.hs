module Lambda where

import Data.List
import Expr

-- TODO 1.1. find free variables of a Expr

free_vars :: Expr -> [String]
free_vars expr = nub (freeVars expr [])
  where
    freeVars :: Expr -> [String] -> [String]
    freeVars (Variable x) list
      | x `elem` list = []
      | otherwise = [x]
    freeVars (Function x body) list = freeVars body (x : list)
    freeVars (Application e1 e2) list = freeVars e1 list ++ freeVars e2 list

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable x) y e = if x == y then e else Variable x
reduce (Function x body) y e
  | x == y = Function x body
  | x `elem` freeVars e =
      let x' = generateFreshVarName (x : freeVars body ++ freeVars e)
      in Function x' (reduce (renameVar body x x') y e)
  | otherwise = Function x (reduce body y e)
reduce (Application e1 e2) y e = Application (reduce e1 y e) (reduce e2 y e)

generateFreshVarName :: [String] -> String
generateFreshVarName usedVars = generateFreshVarNameUtil usedVars freshVars
  where
    freshVars = [c : show n | n <- [1 ..], c <- ['a' .. 'z']]

generateFreshVarNameUtil :: [String] -> [String] -> String
generateFreshVarNameUtil usedVars (x:xs)
  | x `elem` usedVars = generateFreshVarNameUtil usedVars xs
  | otherwise = x

freeVars :: Expr -> [String]
freeVars (Variable x) = [x]
freeVars (Function x body) = filter (/= x) (freeVars body)
freeVars (Application e1 e2) = freeVars e1 ++ freeVars e2

renameVar :: Expr -> String -> String -> Expr
renameVar (Variable x) old new = if x == old then Variable new else Variable x
renameVar (Function x body) old new = if x == old then Function x body else Function x (renameVar body old new)
renameVar (Application e1 e2) old new = Application (renameVar e1 old new) (renameVar e2 old new)

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable x) = Variable x
stepN (Function x body) = Function x (stepN body)
stepN (Application (Function x body) arg) = reduce body x arg
stepN (Application e1 e2)
  | isApplication e1 = Application (stepN e1) e2
  | otherwise = Application e1 (stepN e2)

isApplication :: Expr -> Bool
isApplication (Application _ _) = True
isApplication _ = False


-- TODO 1.4. perform Normal Evaluation

reduceN :: Expr -> Expr
reduceN expr = case stepN expr of
  expr' -> if expr' == expr then expr' else reduceN expr'


reduceAllN :: Expr -> [Expr]
reduceAllN expr = expr : case stepN expr of
  expr' -> if expr' == expr then [] else reduceAllN expr'

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable x) = Variable x
stepA (Function x body) = Function x (stepA body)
stepA (Application (Function x body) arg)
  | isNormalForm body && isNormalForm arg = reduce body x arg
  | isNormalForm arg = reduce body x arg
  | isNormalForm body = Application (Function x body) (stepA arg)
  | otherwise = reduce body x arg
stepA (Application e1 e2)
  | isNormalForm e1 = Application e1 (stepA e2)
  | otherwise = Application (stepA e1) e2

isNormalForm :: Expr -> Bool
isNormalForm (Variable _) = True
isNormalForm (Function _ body) = isNormalForm body
isNormalForm (Application (Function _ _) _) = False
isNormalForm (Application e1 e2) = isNormalForm e1 && isNormalForm e2 && isNormalForm e1


-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr = case stepA expr of
  expr' -> if expr' == expr then expr' else reduceA expr'


reduceAllA :: Expr -> [Expr]
reduceAllA expr = expr : case stepA expr of
  expr' -> if expr' == expr then [] else reduceAllA expr'




-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
