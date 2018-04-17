{-# LANGUAGE GADTs #-}

module Block1 where

import Control.Applicative (liftA2)

type ArithmeticError = String

data Expr a
  where
    Const :: (Num a) => a -> Expr a
    Sum   :: (Num a) => Expr a -> Expr a -> Expr a
    Sub   :: (Num a) => Expr a -> Expr a -> Expr a
    Mul   :: (Num a) => Expr a -> Expr a -> Expr a
    Div   :: (RealFrac a) => Expr a -> Expr a -> Expr a
    Pow   :: (Num a, Integral b) => Expr a -> Expr b -> Expr a

eval :: Expr a -> Either ArithmeticError a
eval (Const a) = Right a
eval (Sum x y) = liftA2 (+) (eval x) (eval y)
eval (Sub x y) = liftA2 (-) (eval x) (eval y)
eval (Mul x y) = liftA2 (*) (eval x) (eval y)
eval (Div x y) = eval y >>= (\k -> if k == 0
                 then Left "Division by zero"
                 else eval x >>= (\t -> Right (t / k)))
eval (Pow x y) = eval y >>= (\k -> if k < 0
                 then Left "Negative arg in exponent"
                 else eval x >>= (\t -> Right (t ^ k)))
 
data IntExpr = IntConst Int
             | IntSum IntExpr IntExpr
             | IntSub IntExpr IntExpr
             | IntMul IntExpr IntExpr
             | IntDiv IntExpr IntExpr
             | IntPow IntExpr IntExpr
    deriving (Show)

evalInt :: IntExpr -> Either ArithmeticError Int
evalInt (IntConst a) = Right a
evalInt (IntSum x y) = liftA2 (+) (evalInt x) (evalInt y)
evalInt (IntSub x y) = liftA2 (-) (evalInt x) (evalInt y)
evalInt (IntMul x y) = liftA2 (*) (evalInt x) (evalInt y)
evalInt (IntDiv x y) = evalInt y >>= (\k -> if k == 0
                 then Left "Division by zero"
                 else evalInt x >>= (\t -> Right (t `div` k)))
evalInt (IntPow x y) = evalInt y >>= (\k -> if k < 0
                 then Left "Negative arg in exponent"
                 else evalInt x >>= (\t -> Right (t ^ k)))