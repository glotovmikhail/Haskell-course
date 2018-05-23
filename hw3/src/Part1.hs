{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Part1
       ( Identifier
       , Value
       , Env
       , Expr (..)
       , ExprException (..)
       , Eval (..)
       , eval
       , doEval
       ) where

import qualified Data.Map  as Map
import qualified Data.Text as DText

import           Control.Exception.Base (ArithException (DivideByZero))
import           Control.Monad.Except
import           Control.Monad.Reader


type Identifier = DText.Text
type Value      = Integer
type Env        = Map.Map Identifier Value

data Expr = Lit Value
          | Var Identifier
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Identifier Expr Expr
      deriving (Show)

data ExprException = UnboundVarException { getVarIdentifier :: Identifier
                                         , getEnv           :: Env
                                         }
                   | ArithmeticException { getExpr      :: Expr
                                         , getEnv       :: Env
                                         , getException :: ArithException
                                         }

instance Show ExprException where
    show (UnboundVarException var env) =
        DText.unpack var ++ " is free [environment: " ++ show env ++ "]"
    show (ArithmeticException expr env ex) =
        show ex ++ " in " ++ show expr ++ " [environment: " ++ show env ++ "]"

newtype Eval m a = Eval { runEval :: ExceptT ExprException (ReaderT Env m) a }
    deriving (Functor, Applicative, Monad, MonadError ExprException, MonadReader Env)

instance MonadTrans Eval where
    lift = Eval . lift . lift

eval :: (MonadError ExprException m, MonadReader Env m) => Expr -> m Value
eval (Lit x) = return x
eval (Var x) = do
    env <- ask
    case Map.lookup x env of
        Just v  -> return v
        Nothing -> throwError $ UnboundVarException x env
eval (Add l r) = do
    lhs <- eval l
    rhs <- eval r
    return $ lhs + rhs
eval (Sub l r) = do
    lhs <- eval l
    rhs <- eval r
    return $ lhs - rhs
eval (Mul l r) = do
    lhs <- eval l
    rhs <- eval r
    return $ lhs * rhs
eval exp@(Div l r) = do
    lhs <- eval l
    rhs <- eval r
    if rhs == 0 
    then do
          env <- ask
          throwError $ ArithmeticException exp env DivideByZero
    else
        return $ lhs `div` rhs
eval (Let v a e) = do
    newval <- eval a
    local (Map.insert v newval) (eval e)

doEval :: Monad m => Env -> Eval m a -> m (Either ExprException a)
doEval env evaluation = runReaderT (runExceptT (runEval evaluation)) env