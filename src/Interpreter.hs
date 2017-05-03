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

class Evaluable b where
    evaluate :: b -> Interpreter Value

instance Evaluable Block where
    evaluate (Block stmts) = evaluate stmts

instance Evaluable [Stmt] where
    evaluate [] = return Void'
    evaluate [stmt] = evaluate stmt
    evaluate (stmt:stmts) = do
        evaluate stmt
        evaluate stmts

instance Evaluable Stmt where
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

instance Evaluable Expr where
    evaluate expr = case expr of
        EVar ident -> do
            result <- gets $ Map.lookup ident
            maybe (throwError $ "Unbound variable " ++ show ident) return result
            -- case result of
            --     Nothing -> throwError "Unbound variable "
            --     Just v -> return v
        ELitInt integer -> return $ Number integer
        ELitTrue -> return $ Boolean True
        ELitFalse -> return $ Boolean False
        --     EApp ident exprs -> failure x
        --     EString string -> failure x
        --     Cond expr block -> failure x
        --     CondElse expr block1 block2 -> failure x
        --     While expr block -> failure x
        --     BStmt block -> failure x
        --     Closure args expr -> failure x
        -- Neg expr -> negate <$> evaluate expr
        --     Not expr -> failure x
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
                    let op = case addop of {Plus -> (*); Minus -> (-)}
                    in op v1 v2
                _ -> throwError $ "type error: " ++ show m1 ++ " and " ++ show m2
        ERel expr1 relop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> return $ Boolean $
                    let op = case relop of {LTH -> (<); LE -> (<=); GTH -> (>); GE -> (>);
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
