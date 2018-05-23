{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Part3
       ( CommandType (..)
       , VarException (..)
       , Command (..)
       , doCommand
       , declare
       , assign
       , evalCommand
       , evalCommands
       , command
       , commands
       ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Text            as DText (Text, pack)
import qualified Data.Text.IO     as DText.IO (getLine)
import           Data.Void            (Void)
import           Text.Megaparsec      (ParseError, Token, runParserT, (<|>), between, sepEndBy)

import           Part1
import           Part2

type CommandScope = [CommandType]

data CommandType = Declaration Identifier Expr
                 | Assignment  Identifier Expr
                 | Print Expr
                 | Read Identifier
                 | For Expr Expr CommandScope
              deriving (Show)

data VarException = MultipleDeclarationException Identifier
                  | NotInScopeException          Identifier
                  | EvalException                ExprException
                  | ParseException               (ParseError (Token DText.Text) Void)
              deriving (Show)

newtype Command m a = Command { runCommand :: ExceptT VarException (StateT Env m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Env, MonadError VarException)

instance MonadTrans Command where
    lift = Command . lift . lift

doCommand :: Monad m => Env -> Command m a -> m (Either VarException a)
doCommand env com = evalStateT (runExceptT (runCommand com)) env


declare :: (MonadError VarException m, MonadState Env m) => Identifier -> Value -> m ()
declare name = doAssignment (not . Map.member name) MultipleDeclarationException name

assign :: (MonadError VarException m, MonadState Env m) => Identifier -> Value -> m ()
assign name = doAssignment (Map.member name) NotInScopeException name

doAssignment :: ( MonadError VarException m
                , MonadState Env m
                )
             => (Env -> Bool)
             -> (Identifier -> VarException)
             -> Identifier
             -> Value
             -> m ()
doAssignment envCheck errorProducer name val = do
    env <- get
    if envCheck env 
    then modify (Map.insert name val)
    else throwError $ errorProducer name

evalOn :: ( MonadError VarException m
          , MonadState Env m
          , MonadIO m
          )
       => (Value -> m ())
       -> Expr
       -> m ()
evalOn func ex = do
    env <- get
    res <- doEval env $ eval ex
    case res of
        Left  e -> throwError $ EvalException e
        Right v -> func v

evalCommand :: ( MonadError VarException m
               , MonadState Env m
               , MonadIO m
               )
            => CommandType
            -> m ()
evalCommand (Declaration name ex) = evalOn (declare name) ex
evalCommand (Assignment name ex)  = evalOn (assign name) ex
evalCommand (Print ex)            = evalOn (liftIO . print) ex
evalCommand (Read name) = do
    line <- liftIO DText.IO.getLine
    res  <- runParserT expr "read" line
    case res of
        Left  e -> throwError $ ParseException e
        Right r -> evalOn (modify . Map.insert name) r
evalCommand (For from to scope) = do
    env   <- get
    fromV <- doEval env $ eval from
    toV   <- doEval env $ eval to
    case (fromV, toV) of
        (Left e, _) -> throwError $ EvalException e
        (_, Left e) -> throwError $ EvalException e
        (Right l, Right r) -> replicateM_ (fromIntegral (r - l)) $ do
            env' <- get
            evalCommands scope
            env'' <- get
            put $ Map.differenceWith (\_ rhs -> Just rhs) env' env''

evalCommands :: ( MonadError VarException m
                , MonadState Env m
                , MonadIO m
                )
             => CommandScope
             -> m ()
evalCommands = mapM_ evalCommand

command :: ExprParser m CommandType
command  =  parseDeclaration
        <|> parseAssignment
        <|> parsePrint
        <|> parseRead
        <|> parseFor

parseDeclaration :: ExprParser m CommandType
parseDeclaration = do
    reservedWord $ DText.pack "mut"
    uncurry Declaration <$> parseAssignment'

parseAssignment :: ExprParser m CommandType
parseAssignment = uncurry Assignment <$> parseAssignment'

parsePrint :: ExprParser m CommandType
parsePrint = do
    _ <- symbol $ DText.pack "<"
    Print <$> expr

parseRead :: ExprParser m CommandType
parseRead = do
    _ <- symbol $ DText.pack ">"
    Read <$> identifier

parseFor :: ExprParser m CommandType
parseFor = do
    reservedWord $ DText.pack "for"
    from <- expr
    reservedWord $ DText.pack "to"
    to <- expr
    scope <- between (symbol $ DText.pack "{") (symbol $ DText.pack "}") commands
    return $ For from to scope

parseAssignment' :: ExprParser m (Identifier, Expr)
parseAssignment' = do
    name <- identifier
    eq
    ex   <- expr
    return (name, ex)

commands :: ExprParser m [CommandType]
commands = command `sepEndBy` spaces'