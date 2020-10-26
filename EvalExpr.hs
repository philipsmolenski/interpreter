-- This module contains functions necessary for expression evaluation

module EvalExpr where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Big.AbsBig
import DataStructures

-- Gets memory location by name
getLoc :: Var -> InterpreterMonad (Loc)
getLoc var@(Ident id) = do
  ienv <- asks ienv
  let envval = var `M.lookup` ienv
  case envval of 
    Nothing -> throwError $ "Variable " ++ id ++ " is undefined"
    (Just val) -> case val of
      (EnvLoc loc) -> return loc 
      _ -> throwError $ "Variable " ++ id ++ " is undefined"

-- Gets variable value by name
getValue :: Var -> InterpreterMonad (Val)
getValue var@(Ident id) = do
  loc <- getLoc var
  store <- gets store
  let val = loc `M.lookup` store
  case val of 
    (Just (Some n)) -> return n
    _ -> throwError $ "Variable " ++ id ++ " declared but uninitialized"

-- Gets procedure by name
getProc :: Var -> InterpreterMonad (Proc)
getProc var@(Ident id) = do 
  ienv <- asks ienv
  let envval = var `M.lookup` ienv
  case envval of 
    Nothing -> throwError $ "Function " ++ id ++ " is undefined"
    (Just val) -> case val of 
      (EnvProc proc) -> return proc
      _-> throwError $ "Function " ++ id ++ " is undefined"

-- Gets weight of exception by its name
getWeight :: Var -> InterpreterMonad (Weight)
getWeight var@(Ident id) = do
  ienv <- asks ienv
  let envval = var `M.lookup` ienv
  case envval of 
    Nothing -> throwError $ "Exception " ++ id ++ " is undefined"
    (Just val) -> case val of
      (EnvWeight weight) -> return weight 
      _ -> throwError $ "Exception " ++ id ++ " is undefined"

-- Used for division and modulo
performDangerousOp :: Integer -> Integer -> (Integer -> Integer -> Integer) -> InterpreterMonad Val
performDangerousOp _ 0 _ = throwError "Division by 0" 
performDangerousOp val1 val2 op = return $ VInt $ val1 `op` val2

-- Core funtion of this module. Takes expression and returns its value
-- There is also type checking but those errors should not be thrown because
-- the types are already checked before in static type checking phase.
evalExpr :: Expr -> InterpreterMonad Val
evalExpr (ELitInt val) = return (VInt val)
evalExpr (ELitTrue) = return (VBool True)
evalExpr (ELitFalse) = return (VBool False)
evalExpr (EString str) = return (VString str)

evalExpr (EVar id) = do 
  val <- getValue id
  return val

evalExpr (Neg exp) = do 
  val <- evalExpr exp
  case val of
    (VThrow id w) -> return $ VThrow id w
    (VInt n) -> return (VInt $ -n)
    _ -> throwError "Negation of non-integer argument"

evalExpr (Not exp) = do 
  val <- evalExpr exp 
  case val of
    (VThrow id w) -> return $ VThrow id w
    (VBool b) -> return (VBool $ not b)
    _ -> throwError "Not operator applied to non-boolean argument"

evalExpr (EMul exp1 op exp2) = do
  val1 <- evalExpr exp1
  val2 <- evalExpr exp2
  case (val1, val2) of
    (VThrow id w, _) -> return $ VThrow id w
    (_, VThrow id w) -> return $ VThrow id w
    (VInt n1, VInt n2) -> case op of
      Times -> return $ VInt $ n1 * n2
      Div -> performDangerousOp n1 n2 div
      Mod -> performDangerousOp n1 n2 mod
    (_,_) -> throwError "Mul operator applied to non-integer arguments"

evalExpr (EAdd exp1 op exp2) = do
  val1 <- evalExpr exp1
  val2 <- evalExpr exp2
  case (val1, val2) of 
    (VThrow id w, _) -> return $ VThrow id w
    (_, VThrow id w) -> return $ VThrow id w
    (VInt n1, VInt n2) -> case op of
      Plus -> return $ VInt $ n1 + n2
      Minus -> return $ VInt $ n1 - n2
    (_,_) -> throwError "Mul operator applied to non-integer arguments"

evalExpr (ECon exp1 exp2) = do
  val1 <- evalExpr exp1
  val2 <- evalExpr exp2
  case (val1, val2) of 
    (VThrow id w, _) -> return $ VThrow id w
    (_, VThrow id w) -> return $ VThrow id w
    (VString s1, VString s2) -> return $ VString $ s1 ++ s2
    (_,_) -> throwError "Concat operator applied to non-string arguments"    

-- == and != operations are available for bool and int values
-- Remaining values are available for integers only.
evalExpr (ERel exp1 op exp2) = do 
  val1 <- evalExpr exp1
  val2 <- evalExpr exp2
  case (val1, val2) of 
    (VThrow id w, _) -> return $ VThrow id w
    (_, VThrow id w) -> return $ VThrow id w
    (VInt n1, VInt n2) -> case op of
      LTH -> return $ VBool $ n1 < n2
      LE -> return $ VBool $ n1 <= n2
      GTH -> return $ VBool $ n1 > n2
      GE -> return $ VBool $ n1 >= n2
      EQU -> return $ VBool $ n1 == n2
      NE -> return $ VBool $ n1 /= n2
    (VBool b1, VBool b2) -> case op of
      EQU -> return $ VBool $ b1 == b2
      NE -> return $ VBool $ b1 /= b2
      _ -> throwError "Rel operator for integers applied to boolean arguments"
    (_,_) -> throwError "Rel operator applied to non-boolean and non-integer arguments"

evalExpr (EAnd exp1 exp2) = do
  val1 <- evalExpr exp1
  val2 <- evalExpr exp2
  case (val1, val2) of 
    (VThrow id w, _) -> return $ VThrow id w
    (_, VThrow id w) -> return $ VThrow id w
    (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
    (_,_) -> throwError "AND operator applied to non-boolean arguments"    

evalExpr (EOr exp1 exp2) = do
  val1 <- evalExpr exp1
  val2 <- evalExpr exp2
  case (val1, val2) of 
    (VThrow id w, _) -> return $ VThrow id w
    (_, VThrow id w) -> return $ VThrow id w
    (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
    (_,_) -> throwError "OR operator applied to non-boolean arguments"

evalExpr (EApp id exps) = do
  proc <- getProc id
  status <- proc exps
  case status of 
    OK -> return VVoid
    Continue -> throwError "Continue statement without loop"
    Break -> throwError "Break statement without loop"
    Return val -> return val 
    Exception id w -> return $ VThrow id w
