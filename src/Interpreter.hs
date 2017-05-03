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

doExpr :: Expr -> ExprM Integer  -- TODO generalize to any type
doExpr expr = case expr of
    EVar ident -> return 0
    ELitInt integer -> return integer
--     ELitTrue -> failure x
--     ELitFalse -> failure x
--     EApp ident exprs -> failure x
--     EString string -> failure x
--     Cond expr block -> failure x
--     CondElse expr block1 block2 -> failure x
--     While expr block -> failure x
--     BStmt block -> failure x
--     Closure args expr -> failure x
    Neg expr -> negate <$> doExpr expr
--     Not expr -> failure x
    EMul expr1 mulop expr2 -> case mulop of
        Times -> (*) <$> doExpr expr1 <*> doExpr expr2
        Div -> div <$> doExpr expr1 <*> doExpr expr2
        Mod -> mod <$> doExpr expr1 <*> doExpr expr2
    EAdd expr1 addop expr2 -> case addop of
        Plus -> (+) <$> doExpr expr1 <*> doExpr expr2
        Minus -> (-) <$> doExpr expr1 <*> doExpr expr2
--     ERel expr1 relop expr2 -> failure x
--     EAnd expr1 expr2 -> failure x
--     EOr expr1 expr2 -> failure x


evalExpr :: ExprM a -> Env -> Either String a
evalExpr = runReader . runExceptT . evalExpr'

foo :: IO ()
foo = putStrLn "foo"
