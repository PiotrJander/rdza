{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

-- import qualified AbsRdza as Abs
import AbsRdza


type Identifier = String
type Env = Map.Map Identifier Int

newtype StmtM a = StmtM {
  execStatement' :: ExceptT String (StateT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadState Env, MonadError String)

-- doStmt :: Abs.Stmt -> StmtM ()
doStmt :: Stmt -> StmtM ()
doStmt stmt = case stmt of
    Decl ident expr -> return ()
    Ass ident expr -> return ()
    Ret expr -> return ()
    VRet -> return ()
    SExp expr -> return ()
--     Abs.Decl ident expr -> return ()
--     Abs.Ass ident expr -> return ()
--     Abs.Ret expr -> return ()
--     Abs.VRet -> return ()
--     Abs.SExp expr -> return ()

execStatement :: StmtM a -> Env -> Either String a
execStatement = evalState . runExceptT . execStatement'

newtype ExprM a = ExprM {
  evalExpr' :: ExceptT String (ReaderT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

-- doExpr :: Abs.Expr -> ExprM a
-- doExpr expr = case expr of
--     Abs.EVar

evalExpr :: ExprM a -> Env -> Either String a
evalExpr = runReader . runExceptT . evalExpr'

foo :: IO ()
foo = putStrLn "foo"
