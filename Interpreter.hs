-- Module used to interpret program statements.

module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Big.AbsBig
import DataStructures
import EvalExpr

runInterpreterMonad :: InterpreterMonad () -> IO (Either Error ())
runInterpreterMonad m = runExceptT $ evalStateT (runReaderT m emptyEnv) emptyState

runInterpreterProgram :: Program -> InterpreterMonad ()
runInterpreterProgram (Program l) = do
  status <- execBlock l
  case status of 
    OK -> return ()
    Continue -> throwError "Continue statement outside loop"
    Break -> throwError "Break statement outside loop"
    Return _ -> throwError "Return statement outside function"
    -- this below should never happen
    Exception _ _ -> throwError "Unknown exception occured"

-- gets new memory location
newKey :: Store -> Loc
newKey m = if (M.null m) then 0 else fst (M.findMax m) + 1

getFreeLoc :: InterpreterMonad Loc 
getFreeLoc = do 
  store <- gets store
  let loc = newKey store
  return $ newKey store

-- Checks if there is default argument preceeding non-default argument
-- Bool arg indicates whether default argument already occured
validateArgsHelper ::[Arg] -> Bool -> InterpreterMonad ()
validateArgsHelper [] _ = return ()
validateArgsHelper (h:t) False = case h of
  DefArg _ _ _-> validateArgsHelper t True
  _ -> validateArgsHelper t False
validateArgsHelper (h:t) True = case h of
  DefArg _ _ _-> validateArgsHelper t True
  _ -> throwError $ "Default argument preceeding non-default argument"

validateArgs :: [Arg] -> InterpreterMonad ()
validateArgs args = validateArgsHelper args False

-- checks if it is possible to change value of this variable
check_ronly :: Var -> InterpreterMonad ()
check_ronly var@(Ident str) = do 
  ronly <- asks ronly
  let b = var `M.lookup` ronly
  case b of 
    (Just True) -> throwError $ "Cannot override read-only value: " ++ str 
    _-> return ()

-- creates procedure that is memorized in procedure environment
createProc :: Type -> Ident -> Bool -> [Arg] -> Env -> [Stmt] -> Proc 
createProc typ id dyn args stat_env stmts exps = do
  case dyn of 
    True -> performDynProc typ id args exps stmts
    False -> do 
      let rec_env = mod_penv id (createProc typ id dyn args stat_env stmts) stat_env
      performStatProc typ id args exps rec_env stmts 

-- Performs procedure declared in dynamic mode
performDynProc :: Type -> Ident -> [Arg] -> [Expr] -> [Stmt] -> InterpreterMonad Status
performDynProc typ (Ident s) [] [] stmts = do 
  status <- execBlock stmts
  case (status == OK && typ /= Void) of
    True -> throwError $ "Non-void function " ++ s ++ " terminates without return statement"
    False -> return status

performDynProc _ (Ident s) [] (exp:exps) _ = throwError $ ("Too many arguments in function " ++ s) 

performDynProc typ (Ident s) (arg:args) [] stmts = case arg of 
  (DefArg _ id exp) -> do 
    xval <- evalExpr exp 
    case xval of 
      (VThrow i w) -> return $ (Exception i w)
      val -> do 
        new_loc <- getFreeLoc
        modify (mod_state new_loc (Some val))
        local (mod_venv id new_loc) $ performDynProc typ (Ident s) args [] stmts

  _ -> throwError $ ("Not enough arguments in function " ++ s)

performDynProc typ (Ident s) (arg:args) (exp:exps) stmts = do
  xval <- evalExpr exp 
  case xval of 
    (VThrow i w) -> return $ (Exception i w)
    val -> case arg of 
      (DefArg _  id _) -> do 
        new_loc <- getFreeLoc
        modify (mod_state new_loc (Some val))
        local (mod_venv id new_loc) $ performDynProc typ (Ident s) args exps stmts
      (ValArg _ id) -> do
        new_loc <- getFreeLoc
        modify (mod_state new_loc (Some val))
        local (mod_venv id new_loc) $ performDynProc typ (Ident s) args exps stmts
      (VarArg _ id) -> do 
        case exp of 
          (EVar ident@(Ident str)) -> do
            ronly <- asks ronly
            let b = ident `M.lookup` ronly
            case b of 
              (Just True) -> throwError $ ("Cannot pass read-only variable " ++ str ++ " by reference in dynamic function " ++ s)
              _ -> do
                loc <- getLoc ident
                local (mod_venv id loc) $ performDynProc typ (Ident s) args exps stmts
          _-> throwError $ ("Bad argument (not identificator) passed by reference in function " ++ s)

-- performs function declared in static mode
performStatProc :: Type -> Ident -> [Arg] -> [Expr] -> Env -> [Stmt] -> InterpreterMonad Status
performStatProc typ (Ident s) [] [] stat_env stmts = do 
  status <- local (\env -> stat_env) $ execBlock stmts
  case (status == OK && typ /= Void) of
    True -> throwError $ "Non-void function " ++ s ++ " terminates without return statement"
    False -> return status

performStatProc _ (Ident s) [] (exp:exps) _ _ = throwError $ ("Too many arguments in function " ++ s)

performStatProc typ (Ident s) (arg:args) [] stat_env stmts = case arg of
  (DefArg _ id exp) -> do
    xval <- evalExpr exp
    case xval of 
      (VThrow i w) -> return $ (Exception i w)
      val -> do 
        new_loc <- getFreeLoc
        modify (mod_state new_loc (Some val))
        let new_env = mod_venv id new_loc stat_env
        performStatProc typ (Ident s) args [] new_env stmts

  _ -> throwError $ ("Not enough arguments in function " ++ s)

performStatProc typ (Ident s) (arg:args) (exp:exps) stat_env stmts = do 
  xval <- evalExpr exp 
  case xval of 
    (VThrow i w) -> return $ (Exception i w)
    val -> case arg of 
      (DefArg _ id _) -> do 
        new_loc <- getFreeLoc
        modify (mod_state new_loc (Some val))
        let new_env = mod_venv id new_loc stat_env
        performStatProc typ (Ident s) args exps new_env stmts
      (ValArg _ id) -> do 
        new_loc <- getFreeLoc
        modify (mod_state new_loc (Some val))
        let new_env = mod_venv id new_loc stat_env
        performStatProc typ (Ident s) args exps new_env stmts
      (VarArg _ id) -> do 
        case exp of 
          (EVar ident@(Ident str)) -> do
            ronly <- asks ronly
            let b = ident `M.lookup` ronly
            case b of 
              (Just True) -> throwError $ ("Cannot pass read-only variable " ++ str ++ " by reference in static function " ++ s)
              _ -> do
                loc <- getLoc ident
                let new_env = mod_venv id loc stat_env
                performStatProc typ (Ident s) args exps new_env stmts
          _-> throwError $ ("Bad argument (not identificator) passed by reference in function " ++ s)

-- Executes block of statements. Used for statements that introduce changes
-- visible within the same scope
execBlock :: [Stmt] -> InterpreterMonad Status
execBlock [] = return OK
execBlock (h:t) = case h of
  -- Variable declaration
  (Decl _ it) -> do
    new_loc <- getFreeLoc
    case it of
      -- Declaration without initialization
      (NoInit id) -> do
        modify (mod_state new_loc Null)
        local ((mod_venv id new_loc).(mod_ronly id False)) $ execBlock t 
      -- Declaration with initializatoin
      (Init id expr) -> do 
        xval <- evalExpr expr
        case xval of 
          (VThrow i w) -> return $ (Exception i w)
          val -> do 
            modify (mod_state new_loc (Some val)) 
            local ((mod_venv id new_loc).(mod_ronly id False)) $ execBlock t
  
  -- Function declaration
  (FunDecl typ id args stmts) -> do
    validateArgs args
    stat_env <- ask
    dyn <- asks isDyn
    let proc = createProc typ id dyn args stat_env stmts
    local ((mod_penv id proc).(mod_ronly id False)) $ execBlock t

  -- Exception declaration
  (ExDecl id) -> do 
    local ((mod_ronly id False).(mod_xenv id Inf)) $ execBlock t

  -- Statements changing declaration mode
  DynOn -> local (mod_isDyn True) $ execBlock t
  DynOff -> local (mod_isDyn False) $ execBlock t

  stmt -> do 
    status <- execStmt stmt
    case status of 
      OK -> execStmt $ BStmt t
      x -> return x

-- Used in for loop. Executes stmt up to the moment then value under
-- name id reaches the value of end
exec_till_end :: Var -> Integer -> Stmt -> InterpreterMonad Status
exec_till_end id end stmt = do
  (VInt cur) <- getValue id 
  if cur == end then return OK else do
    status <- execStmt stmt
    loc <- getLoc id
    modify (mod_state loc $ Some $ VInt $ (+1) $ cur)
    case status of
      OK -> exec_till_end id end stmt
      Continue -> exec_till_end id end stmt
      Break -> return OK
      x -> return x

printExpr :: Expr -> InterpreterMonad ()
printExpr exp = do
  val <- evalExpr exp
  case val of
    (VInt n) -> liftIO $ putStr $ show n
    (VBool b) -> liftIO $ putStr $ show b
    (VString s) -> liftIO $ putStr $ s
    (VVoid) -> return ()

-- Funtion interpreting single statement
execStmt :: Stmt -> InterpreterMonad Status
execStmt Empty = return OK
execStmt (BStmt tab) = execBlock tab

execStmt (Ass id expr) = do 
  check_ronly id
  loc <- getLoc id  
  xval <- evalExpr expr
  case xval of 
    (VThrow i w) -> return $ (Exception i w)
    val -> do 
      modify (mod_state loc  (Some val))
      return OK

execStmt (Incr id@(Ident str)) = do 
  check_ronly id
  loc <- getLoc id 
  val <- getValue id
  case val of
    (VInt n) -> do
      modify (mod_state loc $ Some $ VInt $ (+1) $ n)
      return OK
    _ -> throwError $ "Incr operator applied to non-integer value " ++ str


execStmt (Decr id@(Ident str)) = do 
  check_ronly id
  loc <- getLoc id 
  val <- getValue id
  case val of
    (VInt n) -> do
      modify (mod_state loc $ Some $ VInt $ (n - 1))
      return OK
    _ -> throwError $ "Decr operator applied to non-integer value " ++ str


execStmt (Ret expr) = do
  xval <- evalExpr expr
  case xval of 
    (VThrow i w) -> return $ (Exception i w)
    val -> return $ Return $ val

execStmt VRet =
  return $ Return $ VVoid

execStmt (Cond expr stmt) = do
  val <- evalExpr expr
  case val of
    (VThrow i w) -> return $ (Exception i w)
    (VBool True) -> execStmt stmt
    (VBool False) -> return OK
    _ -> throwError $ "Non-boolean argument " ++ show val ++ " passed as if condition"

execStmt (CondElse expr stmt1 stmt2) = do
  val <- evalExpr expr
  case val of
    (VThrow i w) -> return $ (Exception i w)
    (VBool True) -> execStmt stmt1
    (VBool False) -> execStmt stmt2
    _ -> throwError $ "Non-boolean argument " ++ show val ++ " passed as if condition"

execStmt wh@(While expr stmt) = do
  val <- evalExpr expr
  case val of
    (VThrow i w) -> return $ (Exception i w)
    (VBool True) -> do 
      status <- execStmt stmt
      case status of
        OK -> execStmt wh 
        Continue -> execStmt wh
        Break -> return OK
        x -> return x 
    (VBool False) -> return OK
    _ -> throwError $ "Non-boolean argument " ++ show val ++ " passed as while condition"

-- The loop iterator is a read-only variable
execStmt (For _ id exp1 exp2 stmt) = do
  new_loc <- getFreeLoc
  beg <- evalExpr exp1
  end <- evalExpr exp2
  case (beg, end) of
    (VThrow i w, _) -> return $ (Exception i w)
    (_, VThrow i w) -> return $ (Exception i w)
    (VInt n, VInt m) -> do
      check_ronly id
      modify (mod_state new_loc (Some beg)) 
      status <- local ((mod_venv id new_loc).(mod_ronly id True)) $ exec_till_end id m stmt
      case status of 
        Continue -> return OK
        Break -> return OK
        x -> return x
    _ -> throwError $ "For loop bounds " ++ show beg ++ " " ++ show end ++ " are not integer arguments"

execStmt (SPrint l) = do
  forM_ l printExpr
  liftIO $ putStr "\n"
  return OK

execStmt (Cont) = return Continue
execStmt (Brk) = return Break

execStmt (Call id@(Ident str) exps) = do 
  proc <- getProc id
  status <- proc exps
  case status of 
    OK -> return OK
    Continue -> throwError "Continue statement without loop"
    Break -> throwError "Break statement without loop"
    Return val -> return OK 
    Exception id w -> return $ Exception id w


execStmt (Except try id@(Ident str) exp catch) = do
  val <- evalExpr exp
  case val of 
    (VThrow i w) -> return $ (Exception i w)
    (VInt n) -> do
      cur_weight <- getWeight id
      -- Update the lower boun for which exception with name id is raised
      let new_weight = if ((Finite n) < cur_weight) then (Finite n) else cur_weight
      status <- local (mod_xenv id new_weight) $ execStmt try
      case status of 
        OK -> return OK
        ex@(Exception id2 w) ->
          case (w >= new_weight && id == id2) of
            -- If raised exception has the same name and higher weight then we catch it
            True -> execStmt catch
            -- In other case we pass it further
            False -> return ex
        s -> return s
    _ -> throwError $ "Exception weight must be an integer in exception " ++ str 

execStmt (Throw id@(Ident str) expr) = do 
  val <- evalExpr expr 
  min_weight <- getWeight id
  case val of 
    (VThrow i w) -> return $ (Exception i w)
    (VInt n) -> 
      -- We want to raise exception only if there is block that will catch it
      case ((Finite n) >= min_weight) of 
        True -> return $ Exception id (Finite n)   
        False -> return OK
    _ -> throwError $ "Non-integer weight in throw of exception " ++ str