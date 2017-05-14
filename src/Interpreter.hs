{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import qualified Data.Map as Map
-- import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Identity
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Debug.Trace

-- import qualified AbsRdza as Abs
import AbsRdza

type Env = Map.Map Ident Value
type TypeEnv = Map.Map Ident Type

data Value =
      Number Integer
    | Text String
    -- | Character Char
    | Boolean Bool
    | Void'
    | Callable Ident [Arg] Type Block
    deriving (Eq, Show)

data InterpreterException = ReturnException Value | ErrorException String deriving (Show, Eq)

throwStringError :: (MonadError InterpreterException m) => String -> m a
throwStringError str = throwError $ ErrorException str

throwReturn :: (MonadError InterpreterException m) => Value -> m a
throwReturn value = throwError $ ReturnException value

newtype Interpreter a = Interpreter {
  runInterpreter' :: ExceptT InterpreterException (WriterT [String] (StateT Env Identity)) a
} deriving (Functor, Applicative, Monad, MonadState Env, MonadWriter [String], MonadError InterpreterException)

newtype TypeChecker a = TypeChecker {
  runTypeChecker' :: ExceptT String (StateT TypeEnv Identity) a
} deriving (Functor, Applicative, Monad, MonadState TypeEnv, MonadError String)

runInterpreter :: Interpreter a -> Env -> Either InterpreterException a
-- runInterpreter = evalState . runExceptT . runInterpreter'
runInterpreter int env = fst $ run env
    where run = evalState . runWriterT . runExceptT . runInterpreter' $ int

execInterpreter :: Interpreter a -> Env -> (Either InterpreterException a, [String])
-- runInterpreter = evalState . runExceptT . runInterpreter'
execInterpreter int env = run env
    where run = evalState . runWriterT . runExceptT . runInterpreter' $ int

evalTypeChecker :: TypeChecker a -> TypeEnv -> Either String a
evalTypeChecker = evalState . runExceptT . runTypeChecker'

execTypeChecker :: TypeChecker a -> TypeEnv -> TypeEnv
execTypeChecker = execState . runExceptT . runTypeChecker'

runProgram :: (ProgramNode a) => a -> IO ()
runProgram node = case runInterpreter (evaluate node) Map.empty of
                Left err -> print err
                Right value -> print value

execProgram :: (ProgramNode a) => a -> IO ()
execProgram node = do
    case evalTypeChecker (typecheck node) Map.empty of
        Right _ -> return ()
        Left err -> do
                        hPutStrLn stderr ("Static type checking error: " ++ err)
                        exitFailure
    let (r, w) = execInterpreter (evaluate node) Map.empty
    forM w $ \line -> putStrLn (line ++ "\n")
    case r of
        Left (ErrorException err) -> hPutStrLn stderr err
        Left (ReturnException _) -> hPutStrLn stderr "internal error"
        Right value -> putStrLn ("Program result: " ++ show value)

class ProgramNode b where
    evaluate :: b -> Interpreter Value
    typecheck :: b -> TypeChecker Type

instance ProgramNode Program where
    evaluate (Program tds) = evaluate tds
    typecheck (Program tds) = typecheck tds

instance ProgramNode [TopDef] where
    evaluate [] = return Void'
    evaluate [td] = evaluate td
    evaluate (td:tds) = do
        _ <- evaluate td
        evaluate tds

    typecheck [] = return Void
    typecheck [td] = typecheck td
    typecheck (td:tds) = do
        _ <- typecheck td
        typecheck tds

instance ProgramNode TopDef where
    evaluate (FnDef ident@(Ident name) args returnType block) = do
        let ret = case returnType of
                        ReturnType ret -> ret
                        EmptyReturnType -> Void
        modify $ Map.insert ident $ Callable ident args ret block

        -- if the function is main, execute the program by applying main to ()
        if name == "main"
            then evaluate $ EApp ident []
            else return Void'

    typecheck (FnDef ident@(Ident name) args returnType block) = do
        -- add function parameters to the type env
        forM args $ \(Arg ident type_) -> modify $ Map.insert ident type_
        let params = map (\(Arg _ type_) -> type_) args
        let ret = case returnType of
                        ReturnType ret -> ret
                        EmptyReturnType -> Void
        let fntype = let argTypes = map (\(Arg _ type_) -> type_) args in FnType argTypes ret
        modify $ Map.insert ident fntype
        actualType <- typecheck block
        if ret == actualType
            then return $ fntype
            else throwError $ "type error: function " ++ name ++ " was declared to "
                                ++ "have return type " ++ show ret ++ " but the actual "
                                ++ "return type is " ++ show actualType

instance ProgramNode Block where
    evaluate (Block stmts) = evaluate stmts
    typecheck (Block stmts) = typecheck stmts

instance ProgramNode [Stmt] where
    evaluate [] = return Void'
    evaluate [stmt] = evaluate stmt
    evaluate (stmt:stmts) = do
        evaluate stmt
        evaluate stmts

    typecheck [] = return Void
    typecheck [stmt] = typecheck stmt
    typecheck (stmt:stmts) = do
        typecheck stmt
        typecheck stmts

instance ProgramNode Stmt where
    -- | Evaluation
    evaluate stmt = case stmt of
        Decl ident expr -> do
            value <- evaluate expr
            modify $ Map.insert ident value
            return Void'
        Ass ident expr -> do
            pred <- gets $ Map.member ident
            unless pred $ throwStringError $ "undeclared identifier " ++ show ident
            value <- evaluate expr
            modify $ Map.insert ident value
            return Void'
        Ret expr -> do
            value <- evaluate expr
            throwReturn value
        VRet -> throwReturn Void'
        SExp expr -> evaluate expr
        Print expr -> do
            v <- evaluate expr
            case v of
                Text s -> do
                    tell [s]
                    return Void'
                _ -> throwStringError $ "type error: trying to print not a string"

    -- | Typechecking
    typecheck stmt = case stmt of
        Decl ident expr -> do
            -- TODO deal with maybe
            t <- typecheck expr
            modify $ Map.insert ident t
            return Void
        Ass ident expr -> do
            oldt <- gets $ Map.lookup ident
            newt <- typecheck expr
            if oldt == Just newt
                then return Void
                else throwError "type error: trying to change type of declared variable"
        Ret expr -> typecheck expr  -- TODO agree with function return type
        VRet -> return Void
        SExp expr -> typecheck expr
        Print expr -> do
            t <- typecheck expr
            case t of
                Str -> return Void
                _ -> throwError $ "type error: trying to print a non-string"

instance ProgramNode Expr where
    -- | Evaluation
    evaluate expr = case expr of
        EVar ident -> do
            result <- gets $ Map.lookup ident
            maybe (throwStringError $ "Unbound variable " ++ show ident) return result
        ELitInt integer -> return $ Number integer
        ELitTrue -> return $ Boolean True
        ELitFalse -> return $ Boolean False
        EApp ident@(Ident name) exprs -> do
            if name == "str"
                then do -- built-in converter to strings
                        if length exprs == 1
                            then do
                                v <- evaluate $ head exprs
                                case v of
                                    Number n -> return $ Text $ show n
                                    Boolean b -> return $ Text $ show b
                                    Text t -> return $ Text t
                                    _ -> throwStringError $ show v ++ " can't be converted to string"
                            else throwStringError "function str applied to wrong number of args"
                else do
                    callable <- gets $ Map.lookup ident
                    case callable of
                        Nothing -> throwStringError $ "function " ++ name ++ " not in scope"
                        Just (Callable _ params _ block) -> do
                            forM (zip params exprs) $ \((Arg ident _), expr) -> do
                                value <- evaluate expr
                                modify $ Map.insert ident value
                            let catcher e = case e of
                                            ReturnException v -> return v
                                            ErrorException s -> throwStringError s
                            evaluate block `catchError` catcher
                        Just _ -> throwStringError $ "ident " ++ name ++ " is not a function"
        EString string -> return $ Text $ string
        Cond expr block -> do
            cond <- evaluate expr
            case cond of
                Boolean True -> evaluate block
                Boolean False -> return Void'
                _ -> throwStringError "type error: if"
        CondElse expr block1 block2 -> do
            cond <- evaluate expr
            case cond of
                Boolean True -> evaluate block1
                Boolean False -> evaluate block2
                _ -> throwStringError "type error: if-else"
        loop@(While expr block) -> do
            cond <- evaluate expr
            case cond of
                Boolean True -> do
                    evaluate block
                    evaluate loop
                Boolean False -> return Void'
                _ -> throwStringError "type error: while"
        BStmt block -> evaluate block
        -- Closure args expr -> failure x
        EStrConcat e1 e2 -> do
            v1 <- evaluate e1
            v2 <- evaluate e2
            case (v1, v2) of
                (Text s1, Text s2) -> return $ Text $ s1 ++ s2
                _ -> throwStringError $ "type error: trying to concat two non-strings"
        Neg expr -> do
            result <- evaluate expr
            case result of
                Number n -> return $ Number (-n)
                _ -> throwStringError "type error: negate"
        Not expr -> do
            result <- evaluate expr
            case result of
                Boolean p -> return $ Boolean $ not p
                _ -> throwStringError "type error: not"
        EMul expr1 mulop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> 
                    let op = case mulop of {Times -> (*); Div -> div; Mod -> mod}
                    in if mulop `elem` [Div, Mod] && v2 == (toInteger 0) then throwStringError "division by zero" else return $ Number $ op v1 v2
                _ -> throwStringError $ "type error: " ++ show m1 ++ " and " ++ show m2
        EAdd expr1 addop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> return $ Number $
                    let op = case addop of {Plus -> (+); Minus -> (-)}
                    in op v1 v2
                _ -> throwStringError $ "type error: " ++ show m1 ++ " and " ++ show m2
        ERel expr1 relop expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Number v1, Number v2) -> return $ Boolean $
                    let op = case relop of {LTH -> (<); LE -> (<=); GTH -> (>); GE -> (>=);
                                            EQU -> (==); NE -> (/=)}
                    in op v1 v2
                _ -> throwStringError $ "type error: " ++ show m1 ++ " and " ++ show m2
        EAnd expr1 expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Boolean v1, Boolean v2) -> return $ Boolean $ v1 && v2
                _ -> throwStringError $ "type error: " ++ show m1 ++ " and " ++ show m2
        EOr expr1 expr2 -> do
            m1 <- evaluate expr1
            m2 <- evaluate expr2
            case (m1, m2) of
                (Boolean v1, Boolean v2) -> return $ Boolean $ v1 || v2
                _ -> throwStringError $ "type error: " ++ show m1 ++ " and " ++ show m2

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
        EApp ident@(Ident name) exprs -> do
            if name == "str"
                then do -- built-in converter to strings
                        if length exprs == 1
                            then do
                                v <- typecheck $ head exprs
                                if v `elem` [Str, Int, Bool]
                                    then return Str
                                    else throwError $ show v ++ " can't be converted to string"
                            else throwError "function str applied to wrong number of args"
                else do
                        -- TODO debug state
                        -- s <- get
                        -- trace "state: " $ return $ fmap Map.toList s
                        fntype <- gets $ Map.lookup ident
                        case fntype of
                            Nothing -> throwError $ "function " ++ name ++ " not in scope"
                            Just (FnType paramTypes retType) -> do
                                case length exprs `compare` length paramTypes of
                                    LT -> throwError $ "function " ++ name ++ " applied to too few arguments"
                                    EQ -> return ()
                                    GT -> throwError $ "function " ++ name ++ " applied to too many arguments"
                                forM (zip3 [1..] paramTypes exprs) $ \(i, paramType, expr) -> do
                                    exprType <- typecheck expr
                                    when (paramType /= exprType) $ throwError $ "function " ++ name ++
                                                                    " given incorrect " ++ show i ++ " argument"
                                -- else, type of all parameters and arguments agree
                                return retType
                            Just _ -> throwError $ "type error: " ++ name ++ " is not a function"
        EString string -> return Str
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
        EStrConcat e1 e2 -> do
            t1 <- typecheck e1
            t2 <- typecheck e2
            case (t1, t2) of
                (Str, Str) -> return Str
                _ -> throwError "type error: trying to concatenate two non-strings"
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
    unless (p == Bool) $ throwError $ "typeError: condition in " ++ name
