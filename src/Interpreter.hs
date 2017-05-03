{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter where

import qualified Data.Map as Map
-- import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

-- import qualified AbsRdza as Abs
import AbsRdza


type Env = Map.Map Ident Integer

newtype Interpreter a = Interpreter {
  runInterpreter' :: ExceptT String (StateT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadState Env, MonadError String)

runInterpreter :: Interpreter a -> Env -> Either String a
runInterpreter = evalState . runExceptT . runInterpreter'

class Evaluable a where
    evaluate :: a -> Interpreter Integer

instance Evaluable Block where
    evaluate (Block stmts) = evaluate stmts

instance Evaluable [Stmt] where
    evaluate [] = return 0
    evaluate [stmt] = evaluate stmt
    evaluate (stmt:stmts) = do
        evaluate stmt
        evaluate stmts

instance Evaluable Stmt where
    evaluate stmt = case stmt of
        Decl ident expr -> do
            value <- evaluate expr
            modify $ Map.insert ident value
            return 0
        Ass ident expr -> do
            pred <- gets $ Map.member ident
            unless pred $ throwError $ "undeclared identifier " ++ show ident
            value <- evaluate expr
            modify $ Map.insert ident value
            return 0
        -- TODO effect of return is to stop function execution
        Ret expr -> evaluate expr
        VRet -> return 0
        SExp expr -> return 0
        --     Abs.Decl ident expr -> return ()
        --     Abs.Ass ident expr -> return ()
        --     Abs.Ret expr -> return ()
        --     Abs.VRet -> return ()
        --     Abs.SExp expr -> return ()


instance Evaluable Expr where
    evaluate expr = case expr of
        EVar ident -> do
            result <- gets $ Map.lookup ident
            -- TODO why not maybe?
            case result of
                Nothing -> throwError "Unbound variable "
                Just v -> return v
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
        Neg expr -> negate <$> evaluate expr
        --     Not expr -> failure x
        EMul expr1 mulop expr2 -> case mulop of
            Times -> (*) <$> evaluate expr1 <*> evaluate expr2
            Div -> div <$> evaluate expr1 <*> evaluate expr2
            Mod -> mod <$> evaluate expr1 <*> evaluate expr2
        EAdd expr1 addop expr2 -> case addop of
            Plus -> (+) <$> evaluate expr1 <*> evaluate expr2
            Minus -> (-) <$> evaluate expr1 <*> evaluate expr2
        --     ERel expr1 relop expr2 -> failure x
        --     EAnd expr1 expr2 -> failure x
        --     EOr expr1 expr2 -> failure x
