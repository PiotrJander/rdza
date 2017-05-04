{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter where

import qualified Data.Map as Map
-- import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

-- import qualified AbsRdza as Abs
import AbsRdza


type Env = Map.Map Ident Value

data Value =
      Number Integer
    -- | Str String
    -- | Character Char
    | Boolean Bool
    | Void'
    -- | Function []  -- TODO think what Haskell type corrs to function
    deriving (Eq, Show)

newtype Interpreter a = Interpreter {
  runInterpreter' :: ExceptT String (StateT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadState Env, MonadError String)

runInterpreter :: Interpreter a -> Env -> Either String a
runInterpreter = evalState . runExceptT . runInterpreter'

class ProgramNode b where
    evaluate :: b -> Interpreter Value

instance ProgramNode Program where
    -- | For now, only evaluates the first function.
    -- | FIXME
    evaluate (Program (td:topdefs)) = evaluate td

instance ProgramNode TopDef where
    -- | FIMXME consider args etc
    evaluate (FnDef ident args ret block) = evaluate block

instance ProgramNode Block where
    evaluate (Block stmts) = evaluate stmts

instance ProgramNode [Stmt] where
    evaluate [] = return Void'
    evaluate [stmt] = evaluate stmt
    evaluate (stmt:stmts) = do
        evaluate stmt
        evaluate stmts

instance ProgramNode Stmt where
    evaluate stmt = case stmt of
        Decl ident expr -> do
            value <- evaluate expr
            modify $ Map.insert ident value
            return Void'
        Ass ident expr -> do
            pred <- gets $ Map.member ident
            unless pred $ throwError $ "undeclared identifier " ++ show ident
            value <- evaluate expr
            modify $ Map.insert ident value
            return Void'
        Ret expr -> evaluate expr
        VRet -> return Void'
        SExp expr -> evaluate expr

instance ProgramNode Expr where
    evaluate expr = case expr of
        EVar ident -> do
            result <- gets $ Map.lookup ident
            maybe (throwError $ "Unbound variable " ++ show ident) return result
        ELitInt integer -> return $ Number integer
        ELitTrue -> return $ Boolean True
        ELitFalse -> return $ Boolean False
        -- EApp ident exprs -> failure x
        -- EString string -> return $ String' $ string
        Cond expr block -> do
            cond <- evaluate expr
            case cond of
                Boolean True -> evaluate block
                Boolean False -> return Void'
                _ -> throwError "type error: if"
        CondElse expr block1 block2 -> do
            cond <- evaluate expr
            case cond of
                Boolean True -> evaluate block1
                Boolean False -> evaluate block2
                _ -> throwError "type error: if-else"
        loop@(While expr block) -> do
            cond <- evaluate expr
            case cond of
                Boolean True -> do
                    evaluate block
                    evaluate loop
                Boolean False -> return Void'
                _ -> throwError "type error: while"
        BStmt block -> evaluate block
        -- Closure args expr -> failure x
        Neg expr -> do
            result <- evaluate expr
            case result of
                Number n -> return $ Number (-n)
                _ -> throwError "type error: negate"
        Not expr -> do
            result <- evaluate expr
            case result of
                Boolean p -> return $ Boolean $ not p
                _ -> throwError "type error: not"
        EMul expr1 mulop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> return $ Number $
                    let op = case mulop of {Times -> (*); Div -> div; Mod -> mod}
                    in op v1 v2
                _ -> throwError $ "type error: " ++ show m1 ++ " and " ++ show m2
        EAdd expr1 addop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> return $ Number $
                    let op = case addop of {Plus -> (+); Minus -> (-)}
                    in op v1 v2
                _ -> throwError $ "type error: " ++ show m1 ++ " and " ++ show m2
        ERel expr1 relop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> return $ Boolean $
                    let op = case relop of {LTH -> (<); LE -> (<=); GTH -> (>); GE -> (>=);
                                            EQU -> (==); NE -> (/=)}
                    in op v1 v2
                _ -> throwError $ "type error: " ++ show m1 ++ " and " ++ show m2
        EAnd expr1 expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Boolean v1, Boolean v2) -> return $ Boolean $ v1 && v2
                _ -> throwError $ "type error: " ++ show m1 ++ " and " ++ show m2
        EOr expr1 expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Boolean v1, Boolean v2) -> return $ Boolean $ v1 || v2
                _ -> throwError $ "type error: " ++ show m1 ++ " and " ++ show m2
