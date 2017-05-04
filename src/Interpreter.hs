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
type TypeEnv = Map.Map Ident Type

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

newtype TypeChecker a = TypeChecker {
  runTypeChecker' :: ExceptT String (StateT TypeEnv Identity) a
} deriving (Functor, Applicative, Monad, MonadState TypeEnv, MonadError String)

runInterpreter :: Interpreter a -> Env -> Either String a
runInterpreter = evalState . runExceptT . runInterpreter'

runTypeChecker :: TypeChecker a -> TypeEnv -> Either String a
runTypeChecker = evalState . runExceptT . runTypeChecker'

runProgram :: (ProgramNode a) => a -> IO ()
runProgram node = case runInterpreter (evaluate node) Map.empty of
                Left err -> print err
                Right value -> print value

class ProgramNode b where
    evaluate :: b -> Interpreter Value
    typecheck :: b -> TypeChecker Type

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
    -- | Evaluation
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

    -- | Typechecking
    evaluate stmt = case stmt of
        Decl ident expr -> do
            t <- typecheck expr
            modify $ Map.insert ident t
            return Void
        Ass ident expr -> do
            oldt <- gets $ Map.lookup ident
            newt <- typecheck expr
            if oldt == newt
                then return Void
                else throwError "type error: assignment"
        Ret expr -> typecheck expr  -- TODO agree with function return type
        VRet -> return Void
        SExp expr -> typecheck expr

instance ProgramNode Expr where
    -- | Evaluation
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

    -- | Typechecking
    typecheck expr = case expr of
        EVar ident -> do
            mt <- gets $ Map.lookup ident
            case mt of
                Just t -> return t
                Nothing -> throwError "type error: unbound variable"
        ELitInt _ -> return Int
        ELitTrue -> return Bool
        ELitFalse -> return Bool
        -- EApp ident exprs -> failure x
        -- EString string -> return $ String' $ string
        Cond expr block -> do
            conditionTypecheck expr "if"
            typecheck block
            return Void
        CondElse expr block1 block2 -> do
            conditionTypecheck expr "if-else"
            t1 <- typecheck block1
            t2 <- typecheck block2
            if t1 == t2
                then return t1
                else throwError "type error: mismatch in if-else"
        loop@(While expr block) -> do
            conditionTypecheck expr "while-loop"
            typecheck block
            return Void
        BStmt block -> typecheck block
        -- Closure args expr -> failure x
        Neg expr -> do
            t <- typecheck expr
            if t == Int
                then return Int
                else throwError "type error: neg"
        Not expr -> do
            t <- typecheck expr
            if t == Bool
                then return Bool
                else throwError "type error: not"
        EMul expr1 mulop expr2 -> binaryTypecheck expr1 expr2 Int Int "EMul"
        EAdd expr1 addop expr2 -> binaryTypecheck expr1 expr2 Int Int "EAdd"
        ERel expr1 relop expr2 -> binaryTypecheck expr1 expr2 Int Bool "ERel"
        EAnd expr1 expr2 -> binaryTypecheck expr1 expr2 Bool Bool "EAnd"
        EOr expr1 expr2 -> binaryTypecheck expr1 expr2 Bool Bool "EOr"

-- | Type checking binary expressions
binaryTypecheck :: Expr -> Expr -> Type -> Type -> String -> TypeChecker Type
binaryTypecheck e1 e2 tin tout name = do
    t1 <- typecheck e1
    t2 <- typecheck e2
    if t1 == tin && t2 == tin
        then return tout
        else throwError $ "type error: " ++ name

-- | Type checking boolean condition
conditionTypecheck :: Expr -> String -> TypeChecker ()
conditionTypecheck expr name = do
    p <- typecheck expr
    unless p == Bool $ throwError $ "typeError: condition in " ++ name
