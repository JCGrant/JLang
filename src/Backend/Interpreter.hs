module Backend.Interpreter where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Frontend.Parser

type Env = Map.Map String Expr

interpret :: [Stmt] -> IO ()
interpret stmts = interpret' stmts Map.empty

interpret' :: [Stmt] -> Env -> IO ()
interpret' [] _ = return ()
interpret' (ExprStmt expr : rest) env = interpret' rest env
interpret' (Assign str expr : rest) env = interpret' rest env'
  where env' = Map.insert str expr env
interpret' (Print expr : rest) env = do
  print $ eval expr env
  interpret' rest env

eval :: Expr -> Env -> Int
eval (Int n) _              = n
eval (Var s) env            = eval (Maybe.fromJust (Map.lookup s env)) env
eval (Add lExpr rExpr) env  = eval lExpr env + eval rExpr env
eval (Sub lExpr rExpr) env  = eval lExpr env - eval rExpr env
eval (Mul lExpr rExpr) env  = eval lExpr env * eval rExpr env
eval (Div lExpr rExpr) env  = eval lExpr env `div` eval rExpr env
eval (Neg expr) env         = -(eval expr env)