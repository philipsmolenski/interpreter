-- Module containing definitions of data structures used in
-- Interpreter, EvalExpr and TypeCheck modules.

module DataStructures where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Big.AbsBig

-- Data structures for interpreter

data Val = VInt Integer | VString String | VBool Bool | VVoid | VThrow Ident Weight deriving (Eq, Show)

-- Status is the returned value in execStmt function 
-- that helps to control program flow
data Status 
  = OK 
  | Break
  | Continue 
  | Return Val
  | Exception Ident Weight deriving (Eq, Show)

type Error = String
type Var = Ident
type Proc = [Expr] -> InterpreterMonad Status

-- Data type representing exception weights 
-- newly declared exceptions do not catch anything, so their wieight is infinity
data Weight = Finite Integer | Inf deriving (Eq, Show)

-- Abstracion of location in memory
type Loc = Integer

-- Abstraction for variable values, Null for declared but undefined variables
data StoreVal = Some Val | Null

-- Maps memory locations to values
type Store = M.Map Loc StoreVal
-- Controls which variables are read-only
type ROnly = M.Map Var Bool

-- there is one map mapping names to all object types so there is
-- no situation where for example there are function and variable
-- with the same name in one scope.
data EnvVal = EnvLoc Loc | EnvProc Proc | EnvWeight Weight
type IEnv =  M.Map Var EnvVal

data MyState = MyState {
  store :: Store
}

data Env = Env {
  ienv :: IEnv,
  ronly :: ROnly,
  isDyn :: Bool
}

type InterpreterMonad a = ReaderT Env (StateT MyState (ExceptT Error IO)) a

emptyState :: MyState
emptyState = MyState { 
  store = M.empty
}

emptyEnv :: Env
emptyEnv = Env {
  ienv = M.empty,
  ronly = M.empty,
  isDyn = False 
}

-- State and Environment modifiers used in functions local and modify
mod_ienv :: Ident -> EnvVal -> Env -> Env
mod_ienv id envval env = env {ienv = M.insert id envval $ ienv env}

mod_venv :: Ident -> Loc -> Env -> Env
mod_venv id loc = mod_ienv id (EnvLoc loc)

mod_penv :: Ident -> Proc -> Env -> Env
mod_penv id proc = mod_ienv id (EnvProc proc)

mod_xenv :: Ident -> Weight -> Env -> Env
mod_xenv id w = mod_ienv id (EnvWeight w)

mod_state :: Loc -> StoreVal -> MyState -> MyState
mod_state loc storeVal state = state {store = M.insert loc storeVal $ store state}

mod_ronly :: Ident -> Bool -> Env -> Env
mod_ronly id b env = env {ronly = M.insert id b $ ronly env}

mod_isDyn :: Bool -> Env -> Env
mod_isDyn b env = env {isDyn = b}

instance Ord Weight where
  (<=) w1 w2 = case (w1, w2) of
    (_, Inf) -> True
    (Inf, _) -> False
    (Finite a, Finite b) -> a <= b 

-- data structures for type checking --

-- Checks the recursion depth for dynamic functions
-- The value is StaticDep for static functions
data DynDepth = Depth Integer | StaticDep
-- Checks the body of dynamic function
-- The value is StaticBod for static functions
data DynBody = Body [Stmt] | StaticBod

-- Maps variable names to types
type TVenv = M.Map Var Type 
-- Maps functions names to types
type TPenv = M.Map Var Type 
-- Maps function names to argument types
type TArgs = M.Map Var [Arg]
-- Checks if exceptions are already declared
type TXenv = M.Map Var Bool
-- Maps function names to their current recursion depth
type DepEnv = M.Map Var DynDepth
-- Maps function names to their bodies
type BodEnv = M.Map Var DynBody

type TCM a = ReaderT TCMEnv (ExceptT Error IO) a

-- Maximum recursion depth (it seems that if types are ok for 
-- first two recursive calls then they are always ok)
maxDepth :: Integer
maxDepth = 2

data TCMEnv = TCMEnv {
  tvenv :: TVenv,
  tpenv :: TPenv,
  targs :: TArgs,
  txenv :: TXenv,
  -- Controls what type should be returned (Nothing outside function)
  retType :: Maybe (Type, Ident),
  depenv :: DepEnv,
  bodenv :: BodEnv,
  dyn :: Bool
}

emptyTCMEnv :: TCMEnv
emptyTCMEnv = TCMEnv {
  tvenv = M.empty,
  tpenv = M.empty,
  targs = M.empty,
  txenv = M.empty,
  retType = Nothing,
  depenv = M.empty,
  bodenv = M.empty,
  dyn = False
}

-- modifiers for modify and local functions
mod_tvenv :: Var -> Type -> TCMEnv -> TCMEnv
mod_tvenv id typ env = env {tvenv = M.insert id typ $ tvenv env}

mod_tpenv :: Var -> Type -> TCMEnv -> TCMEnv
mod_tpenv id typ env = env {tpenv = M.insert id typ $ tpenv env}

mod_targs :: Var -> [Arg] -> TCMEnv -> TCMEnv
mod_targs id typs env = env {targs = M.insert id typs $ targs env}

mod_txenv :: Var -> Bool -> TCMEnv -> TCMEnv
mod_txenv id b env = env {txenv = M.insert id b $ txenv env}

mod_retType :: (Type, Ident) -> TCMEnv -> TCMEnv 
mod_retType tid env = env {retType = (Just tid)}

mod_dyn :: Bool -> TCMEnv -> TCMEnv
mod_dyn b state = state{dyn = b}

mod_depenv :: Var -> DynDepth -> TCMEnv -> TCMEnv
mod_depenv id dep env = env {depenv = M.insert id dep $ depenv env}

mod_bodenv :: Var -> DynBody -> TCMEnv -> TCMEnv
mod_bodenv id bod env = env {bodenv = M.insert id bod $ bodenv env}
