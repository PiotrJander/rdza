import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
-- import Test.HUnit

import Data.List
import Data.Ord

import AbsRdza
import Interpreter
import qualified Data.Map as Map

main = defaultMain tests

tests :: TestTree
-- tests = testGroup "Tests" [evaluateTests, typecheckTests]
tests = testGroup "Tests" [typecheckTests]

typecheckTests = testGroup "Typecheck tests"
    [
        testCase "If / while condition success" $
        let
            snippet = conditionTypecheck (ERel (ELitInt 2) EQU (ELitInt 3)) "if"
        in evalTypeChecker snippet Map.empty @?= Right ()
        ,
        testCase "If / while condition failure" $
        let
            snippet = conditionTypecheck (ELitInt 6) "if"
        in evalTypeChecker snippet Map.empty @?= Left "typeError: condition in if"
        ,
        testCase "Variables in if block get typechecked" $
        let
            snippet = typecheck $ Cond ELitTrue $ Block [
                Decl (Ident "x") (ELitInt 6)
                ]
        in execTypeChecker snippet Map.empty @?= Map.fromList [(Ident "x", Int)]
        ,
        testCase "Variables in if block get typechecked with failure" $
        let
            snippet = typecheck $ Cond ELitTrue $ Block [
                Ass (Ident "x") (ELitInt 6)
                ]
            result = evalTypeChecker snippet $ Map.fromList [(Ident "x", Bool)]
        in result @?= Left "type error: assignment"
        ,
        testCase "Binary typecheck success" $
        let
            snippet = typecheck $ EAnd ELitTrue ELitFalse
        in evalTypeChecker snippet Map.empty @?= Right Bool
        ,
        testCase "Binary typecheck error" $
        let
            snippet = typecheck $ EAnd ELitTrue (ELitInt 5)
        in evalTypeChecker snippet Map.empty @?= Left "type error: EAnd"
        ,
        testCase "Top level function main" $
        let
            snippet = typecheck $ Program [main]
            main = FnDef (Ident "main") [] (ReturnType Void) block
            block = Block []
        in evalTypeChecker snippet Map.empty @?= Right Void
    ]

evaluateTests = testGroup "Evaluate tests"
    [
        testCase "Evaluate addition expression" $
        let
            snippet = evaluate (EAdd (ELitInt 2) Plus (ELitInt 2))
        in runInterpreter snippet Map.empty @?= Right (Number 4)
        ,
        testCase "Evaluate mult expression" $
        let
            snippet = evaluate (EMul (ELitInt 2) Times (ELitInt 2))
        in runInterpreter snippet Map.empty @?= Right (Number 4)
        ,
        testCase "Throw error in mult expression" $
        let
            snippet = evaluate (EMul (ELitInt 2) Times ELitTrue)
        in runInterpreter snippet Map.empty @?= Left "type error: Number 2 and Boolean True"
        ,
        testCase "Lookup variable" $
        let
            snippet = evaluate (EVar (Ident "foo"))
        in runInterpreter snippet (Map.fromList [(Ident "foo", Number 4)]) @?= Right (Number 4)
        ,
        testCase "Variable declaration" $
        let
            snippet = evaluate $ Block [
                Decl (Ident "x") $ ELitInt 2
                , Ret $ EVar $ Ident "x"
                ]
        in runInterpreter snippet Map.empty @?= Right (Number 2)
        ,
        testCase "Variable declaration and assignment" $
        let
            snippet = evaluate $ Block [
                Decl (Ident "x") $ ELitInt 2,
                Ass (Ident "x") $ EAdd (EVar (Ident "x")) Plus (ELitInt 1),
                Ret $ EVar $ Ident "x"]
        in runInterpreter snippet Map.empty @?= Right (Number 3)
        ,
        -- testCase "This should break" $
        -- let
        --     snippet = evaluate $ EAdd ELitTrue Plus ELitFalse
        -- in runInterpreter snippet Map.empty @?= Right (Number 3)
        -- ,
        testCase "If expr" $
        let
            x = Ident "x"
            var = EVar x
            one = ELitInt 1
            two = ELitInt 2
            snippet = evaluate $ Block [
                Decl x two  -- x := 2
                , SExp $ Cond (ERel var EQU two) (Block [
                    -- this conditional should be taken
                    Ass x $ EAdd var Plus one  -- x := x + 1 = 3
                    ])
                , SExp $ Cond (ERel var EQU two) (Block [
                    -- this conditional should NOT be taken
                    Ass x $ EAdd var Plus one
                    ])
                , Ret var  -- x = 3
                ]
        in runInterpreter snippet Map.empty @?= Right (Number 3)
        ,
        testCase "If-else expr" $
        let
            x = Ident "x"
            y = Ident "y"
            varx = EVar x
            vary = EVar y
            one = ELitInt 1
            two = ELitInt 2
            three = ELitInt 3
            snippet = evaluate $ Block [
                Decl x two  -- x := 2
                , Decl y one  -- y := 1
                , SExp $ CondElse (ERel varx EQU two) (Block [
                    -- this branch should be taken
                    Ass y two  -- y := 2
                    ])
                    (Block [
                    -- this branch shoult NOT be taken
                    Ass y three  -- y := 3
                    ])
                , Ret vary  -- y = 2
                ]
        in runInterpreter snippet Map.empty @?= Right (Number 2)
        ,
        testCase "While loop" $
        let
            x = Ident "x"
            varx = EVar x
            zero = ELitInt 0
            one = ELitInt 1
            ten = ELitInt 10
            snippet = evaluate $ Block [
                Decl x zero  -- x := 1
                , SExp $ While (ERel varx LTH ten) (Block [
                    -- this block should be repeated ten times
                    Ass x $ EAdd varx Plus one  -- x := x + 1
                    ])
                , Ret varx  -- x = 10
                ]
        in runInterpreter snippet Map.empty @?= Right (Number 10)
    ]


-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]
--
-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]
--
-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]
--
-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
