-- This module implements functions used for static type checking phase

module TypeCheck where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Big.AbsBig
import DataStructures
import EvalExpr

runTCM :: TCM () -> IO (Either Error ())
runTCM m = runExceptT $ runReaderT m emptyTCMEnv

runTCMProgram :: Program -> TCM ()
runTCMProgram (Program l) = do
  checkBlock l
  liftIO $ putStrLn $ "Static TypeCheck OK"

-- Checks if exception is already declared
checkTXenv :: Var -> TCM()
checkTXenv var@(Ident id) = do
  txenv <- asks txenv
  let b = var `M.lookup` txenv
  case b of
    Nothing -> throwError $ "Exception " ++ id ++ " is undefined"
    _-> return ()

-- Checks if variable is alredy declared
getVarType :: Var -> TCM Type
getVarType var@(Ident id) = do
  tvenv <- asks tvenv
  let typ = var `M.lookup` tvenv
  case typ of 
    Nothing -> throwError $ "Variable " ++ id ++ " is undefined"
    (Just t) -> return t

-- Check type of procedure by name
getProcType :: Var -> TCM Type
getProcType var@(Ident id) = do
  tpenv <- asks tpenv
  let typ = var `M.lookup` tpenv
  case typ of 
    Nothing -> throwError $ "Function " ++ id ++ " is undefined"
    (Just t) -> return t

-- Gets type of procedure types by procedure name
getProcArgs :: Var -> TCM [Arg]
getProcArgs var@(Ident id) = do
  targs <- asks targs
  let args = var `M.lookup` targs
  case args of 
    Nothing -> throwError $ "Function " ++ id ++ " is undefined"
    (Just t) -> return t

-- Gets body of procedure by its name
getProcBody :: Var -> TCM DynBody
getProcBody var@(Ident id) = do
  bodenv <- asks bodenv
  let body = var `M.lookup` bodenv
  case body of 
    Nothing -> throwError $ "Function " ++ id ++ " is undefined"
    (Just t) -> return t

-- Gets current depth of procedure recursion by its name
getProcDepth :: Var -> TCM Integer
getProcDepth var@(Ident id) = do 
  depenv <- asks depenv
  let dep = var `M.lookup` depenv
  case dep of
    Nothing -> throwError $ "Function " ++ id ++ " is undefined"
    (Just (Depth d)) -> return d 
    -- the result should never be StaticDep because we will check
    -- function is dynamic before using getProcDepth
    (Just StaticDep) -> throwError $ "Unpredicted error occured"

-- Some functions extracting types from data structures
getArgIdent :: Arg -> Ident 
getArgIdent arg = 
  case arg of 
    (ValArg _ id) -> id
    (VarArg _ id) -> id
    (DefArg _ id _) -> id

getArgType :: Arg -> Type 
getArgType arg =
  case arg of 
    (ValArg t _) -> t
    (VarArg t _) -> t
    (DefArg t _ _) -> t

getItemIdent :: Item -> Ident
getItemIdent it = case it of
  (Init id _) -> id 
  (NoInit id) -> id 

-- Declarations of function's arguments before running procedure
assignArgs :: [Arg] -> TCMEnv -> TCMEnv
assignArgs [] env = env 
assignArgs (arg:args) env =  
  let id = getArgIdent arg in
  let typ = getArgType arg in
  assignArgs args (mod_tvenv id typ env)

-- Checks if there is default argument preceeding non-default argument
-- Bool arg indicates whether default argument already occured
validateArgsHelper ::[Arg] -> Bool -> TCM ()
validateArgsHelper [] _ = return ()
validateArgsHelper (h:t) False = case h of
  DefArg _ _ _-> validateArgsHelper t True
  _ -> validateArgsHelper t False
validateArgsHelper (h:t) True = case h of
  DefArg _ _ _-> validateArgsHelper t True
  _ -> throwError $ "Default argument preceeding non-default argument"

validateArgs :: [Arg] -> TCM ()
validateArgs args = validateArgsHelper args False

-- Checks if provided provided expressions are of the same type as
-- function arguments 
checkArgs :: Ident -> [Arg] -> [Expr] -> TCM ()
checkArgs _ [] [] = return ()

-- One can only skip default arguments
checkArgs (Ident str) (arg:args) [] = case arg of 
  (DefArg _ _ _) -> return ()
  _ -> throwError $ "Not enough arguments in function " ++ str

checkArgs (Ident str) [] (exp:exps) =
  throwError $ "Too many arguments in function " ++ str

checkArgs id@(Ident str) (arg:args) (exp:exps) = do
  let typ1 = getArgType arg 
  typ2 <- checkExpr exp 
  case (typ1 == typ2) of
    True -> case arg of
      -- When argument is passed by variable, an identificator 
      -- must be provided during function call.
      (VarArg _ _) -> case exp of 
        (EVar _) -> checkArgs id args exps 
        _ ->  throwError $ ("Bad argument (not identificator) passed by reference in function " ++ str)
      _ -> checkArgs id args exps
    False -> throwError $ ("Bad types of arguments provided to function " ++ str)

-- Checking expression types
checkExpr :: Expr -> TCM Type
checkExpr (ELitInt _) = return Int
checkExpr (ELitTrue) = return Bool
checkExpr (ELitFalse) = return Bool
checkExpr (EString _) = return Str 
checkExpr (EVar id) = getVarType id 

checkExpr (Neg exp) = do
  typ <- checkExpr exp 
  case typ of
    Int -> return Int 
    _ -> throwError "Negation of non-integer argument"

checkExpr (Not exp) = do
  typ <- checkExpr exp 
  case typ of
    Bool -> return Bool
    _ -> throwError "Not operator applied to non-boolean argument"

checkExpr (EMul exp1 _ exp2) = do
  typ1 <- checkExpr exp1
  typ2 <- checkExpr exp2
  case (typ1, typ2) of
    (Int, Int) -> return Int 
    (_,_) -> throwError "Mul operator applied to non-integer arguments"

checkExpr (EAdd exp1 _ exp2) = do
  typ1 <- checkExpr exp1
  typ2 <- checkExpr exp2
  case (typ1, typ2) of 
    (Int, Int) -> return Int
    (_,_) -> throwError "Add operator applied to non-integer arguments"

checkExpr (ECon exp1 exp2) = do
  typ1 <- checkExpr exp1
  typ2 <- checkExpr exp2
  case (typ1, typ2) of 
    (Str, Str) -> return Str
    (_,_) -> throwError "Concat operator applied to non-string arguments"    

checkExpr (ERel exp1 op exp2) = do 
  typ1 <- checkExpr exp1
  typ2 <- checkExpr exp2
  case (typ1, typ2) of 
    (Int, Int) -> return Bool
    (Bool, Bool) -> case op of
      EQU -> return $ Bool
      NE -> return $ Bool
      _ -> throwError "Rel operator for integers applied to boolean arguments"
    (_,_) -> throwError "Rel operator applied to non-boolean and non-integer arguments"

checkExpr (EAnd exp1 exp2) = do
  typ1 <- checkExpr exp1
  typ2 <- checkExpr exp2
  case (typ1, typ2) of 
    (Bool, Bool) -> return $ Bool
    (_,_) -> throwError "AND operator applied to non-boolean arguments"    

checkExpr (EOr exp1 exp2) = do
  typ1 <- checkExpr exp1
  typ2 <- checkExpr exp2
  case (typ1, typ2) of 
    (Bool, Bool) -> return $ Bool
    (_,_) -> throwError "OR operator applied to non-boolean arguments"    

checkExpr (EApp id exps) = do
  typ <- getProcType id 
  args <- getProcArgs id
  checkArgs id args exps
  bod <- getProcBody id
  case bod of 
    -- Static function was already checked during declaration
    StaticBod -> return typ 
    -- Dynamic function is checked if it's recursion depth is <= 2
    Body stmts -> do 
      d <- getProcDepth id
      case (d > maxDepth) of 
        True -> return typ
        False -> do
          local ((assignArgs args).(mod_retType (typ, id)).(mod_depenv id (Depth $ d + 1))) $ checkBlock stmts
          return typ

-- Checks statement blocks. Used for statements that introduce changes
-- visible within the same scope
checkBlock :: [Stmt] -> TCM()
checkBlock [] = return ()
checkBlock (h:t) = case h of
  -- Variable declaration
  (Decl typ it) ->  
    case it of 
      (NoInit id) -> local (mod_tvenv id typ) $ checkBlock t
      (Init id@(Ident str) exp) -> do 
        typ2 <- checkExpr exp 
        case (typ == typ2) of 
          True -> local (mod_tvenv id typ) $ checkBlock t
          False -> throwError $ "Variable " ++ str ++ " initialized with bad type"

  -- function declaration 
  (FunDecl typ id args stmts) -> do
    validateArgs args
    dyn <- asks dyn 
    case dyn of 
      -- Static functions are checked during declaration
      False -> do
        local ((mod_targs id args).(assignArgs args).(mod_tpenv id typ).(mod_retType (typ, id)).
          (mod_depenv id StaticDep).(mod_bodenv id StaticBod)) $ checkBlock stmts
        local ((mod_targs id args).(mod_tpenv id typ).
          (mod_depenv id StaticDep).(mod_bodenv id StaticBod)) $ checkBlock t
        -- For dynamic functions their body and depth is saved and they will be 
        -- checked when called.
      True -> 
        local ((mod_targs id args).(mod_tpenv id typ).
          (mod_depenv id (Depth 0)).(mod_bodenv id (Body stmts))) $ checkBlock t

  -- Exception declaration
  (ExDecl id) -> local (mod_txenv id True) $ checkBlock t 
  
  -- Dynamic declarations control
  (DynOn) -> local (mod_dyn True) $ checkBlock t
  (DynOff) -> local (mod_dyn False) $ checkBlock t

  stmt -> do
    checkStmt stmt
    checkBlock t

-- Checks types of statments
checkStmt :: Stmt -> TCM ()
checkStmt Empty = return ()
checkStmt (BStmt tab) = checkBlock tab

checkStmt (Ass id@(Ident str) exp) = do
  typ1 <- getVarType id
  typ2 <- checkExpr exp 
  case (typ1 == typ2) of
    True -> return ()
    False -> throwError $ ("Cannot assign expression of different type to variable " ++ str)

checkStmt (Incr id@(Ident str)) = do
  typ <- getVarType id 
  case typ of
    Int -> return ()
    _ -> throwError $ "Incr operator applied to non-integer value " ++ str

checkStmt (Decr id@(Ident str)) = do
  typ <- getVarType id 
  case typ of
    Int -> return ()
    _ -> throwError $ "Decr operator applied to non-integer value " ++ str
 
checkStmt (Ret exp) = do 
  typ1 <- checkExpr exp 
  ret <- asks retType
  case ret of
    Nothing -> throwError $ "Return statement outside function"
    Just (typ2, Ident str) -> case (typ1 == typ2) of
      True -> return ()
      False -> throwError $ ("Function " ++ str ++ " returns value of bad type") 

checkStmt (VRet) = do 
  ret <- asks retType
  case ret of
    Nothing -> throwError $ "Return statement outside function"
    Just (typ, Ident str) -> case (typ == Void) of
      True -> return ()
      False -> throwError $ ("Function " ++ str ++ " returns value of bad type")       

checkStmt (Cond exp stmt) = do
  typ <- checkExpr exp 
  case typ of 
    -- The statement is checked regardless of condition value
    Bool -> checkStmt stmt 
    _ -> throwError $ "Non-boolean argument passed as if condition" 

checkStmt (CondElse exp stmt1 stmt2) = do
  typ <- checkExpr exp 
  case typ of 
    Bool -> do 
      -- Both branches are checked regardless of condition value
      checkStmt stmt1
      checkStmt stmt2 
    _ -> throwError $ "Non-boolean argument passed as if condition" 

checkStmt (While exp stmt) = do
  typ <- checkExpr exp 
  case typ of 
    -- Type of while loob body is checked only once
    Bool -> checkStmt stmt
    _ -> throwError $ "Non-boolean argument passed as while condition" 

checkStmt (For typ id beg end stmt) = do
  typ1 <- checkExpr beg
  typ2 <- checkExpr end
  case typ of 
    Int -> case (typ1, typ2) of
      -- Body of for loop is checked only once
      (Int, Int) -> local (mod_tvenv id Int) $ checkStmt stmt 
      _ -> throwError $ "For loop bounds are not integer arguments"
    _ -> throwError $ "Loop index should have type int"

checkStmt (SPrint exps) = 
  forM_ exps checkExpr

checkStmt (Cont) = return ()
checkStmt (Brk) = return ()

checkStmt (Call id exps) = do
  typ <- getProcType id 
  args <- getProcArgs id
  checkArgs id args exps
  bod <- getProcBody id
  case bod of 
    -- Static functions are checked during declaration
    StaticBod -> return () 
    -- Dynamic functions are checked if recursion depth is <= 2
    Body stmts -> do 
      d <- getProcDepth id
      case (d > maxDepth) of 
        True -> return ()
        False -> do
          local ((assignArgs args).(mod_retType (typ, id)).(mod_depenv id (Depth $ d + 1))) $ checkBlock stmts

checkStmt (Except try @id(Ident str) exp catch) = do
  typ <- checkExpr exp 
  case typ of
    Int -> do 
      checkStmt try
      checkStmt catch
    _ -> throwError $ "Exception weight must be an integer in exception " ++ str 

checkStmt (Throw id@(Ident str) expr) = do 
  typ <- checkExpr expr 
  case typ of 
    Int -> return ()
    _ -> throwError $ "Non-integer weight in throw of exception " ++ str

