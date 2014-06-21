-- (1.5 балла)
{-# LANGUAGE TupleSections #-}

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Test.HUnit

import Expr
import Eval
import Utils

getInt :: Eval Value -> Eval Integer
getInt m = do
    v <- m
    case v of
        (I x) -> return x
        _ -> fail "Int expected"
     
getBool :: Eval Value -> Eval Bool
getBool m = do 
    v <- m
    case v of
        (B x) -> return x
        _ -> fail "Bool expected"

if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = do
    c' <- liftM (fromMaybe False) $ try $ getBool c
    if c' then t else fromMaybe (return ()) e

evalExpr :: Expr -> Eval Value
evalExpr (Const v) = return v 
evalExpr (BinOp op a' b') = do
    let a = evalExpr a'
    let b = evalExpr b'
    case op of
       Plus -> liftM2 (\x y -> I $ x + y) (getInt a) (getInt b) 
       Mul -> liftM2 (\x y -> I $ x * y) (getInt a) (getInt b)
       Minus -> liftM2 (\x y -> I $ x - y) (getInt a) (getInt b)
       Less -> liftM2 (\x y -> B $ x < y) (getInt a) (getInt b)
       Greater -> liftM2 (\x y -> B $ x > y) (getInt a) (getInt b)
       Equals -> liftM2 (\x y -> B $ x == y) (getInt a) (getInt b)
       And -> liftM2 (\x y -> B $ x && y) (getBool a) (getBool b)
       Or -> evalOr a b
    where
        evalOr a b = do
            ra <- try $ getBool a
            if (fromMaybe False ra) then return $ B True else liftM (B) $ getBool b 
            
evalExpr (UnOp op v') = do
    let v = evalExpr v'
    case op of
        Not -> liftM (B . not) $ getBool v
        Neg -> liftM (I . negate) $ getInt v

evalExpr (Var s) = getVar s  

evalStatement :: Statement -> Eval ()
evalStatement (If c t e) = if' (evalExpr c) (evalProgram t) (fmap evalProgram e) 
evalStatement r@(While c p) = if' (evalExpr c) (evalProgram p >> evalStatement r) Nothing
evalStatement (Assign v e) = do
   eValue <- evalExpr e
   update v eValue

evalProgram :: Program -> Eval ()
evalProgram = mapM_ $ try . evalStatement    

------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

test1 = not_ (Var "x") ||| Var "y" <<< Const (I 3) &&& Var "z" === Var "y" &&&
    Const (I 5) <<< Var "y" +++ Const (I 7) *** Var "z" +++ Var "y" *** Const (I 3)

test2 = neg (Const $ I 5) +++ neg (Const $ I 3) *** Const (I 2) -.- Const (I 7)

test3 =
    [ "r" $= Const (I 1)
    , While (Var "n" >>> Const (I 0))
        [ "r" $= Var "r" *** Var "n"
        , "n" $= Var "n" -.- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) ~?= 2
        -- 2 ошибки: неизвестная переменная z и несоответствие типов (в том месте, где вычисляется "!x")
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalProgram test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
