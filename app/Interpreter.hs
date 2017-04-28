{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

import qualified AbsRdza as Abs


type Var = String
type Env = Map.Map Var Int

newtype StmtM a = StmtM {
  execStatement' :: ExceptT String (StateT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadState Env, MonadError String)

doStmt :: Abs.Stmt -> StmtM ()
doStmt stmt = case stmt of


execStatement :: StmtM a -> Env -> Either String a
execStatement = evalState . runExceptT . execStatement'

newtype ExprM a = ExprM {
  evalExpr' :: ExceptT String (ReaderT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

evalExpr :: ExprM a -> Env -> Either String a
evalExpr = runReader . runExceptT . evalExpr'


