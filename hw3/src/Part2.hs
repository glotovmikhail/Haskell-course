{-# LANGUAGE TypeFamilies #-}

module Part2
       ( ExprParser
       , symbol
       , lexeme
       , value
       , reservedWords
       , reservedWord
       , identifier
       , eq
       , newLineSpace
       , spaces
       , spaces'
       , lineComment
       , blockComment
       , inBrackets
       , expr
       ) where

import           Part1 (Expr (..), Value)

import qualified Data.Text as DText

import           Data.Char                  (isAlphaNum, isSpace)
import           Data.Functor               (void)
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L



type ExprParser m a = ParsecT Void DText.Text m a

symbol :: DText.Text -> ExprParser m DText.Text
symbol = L.symbol spaces

lexeme :: ExprParser m a -> ExprParser m a
lexeme = L.lexeme spaces

value :: ExprParser m Value
value = lexeme L.decimal

reservedWords :: [DText.Text]
reservedWords = map DText.pack ["mut", "let", "in", "for", "to"]

reservedWord :: DText.Text -> ExprParser m ()
reservedWord w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: ExprParser m DText.Text
identifier = (lexeme . try) (p >>= check)
  where
    p       = DText.cons <$> letterChar <*> takeWhileP Nothing isAlphaNum
    check x = if x `elem` reservedWords
              then fail $ "word " ++ DText.unpack x ++ " can't be an identifier"
              else return x

eq :: ExprParser m ()
eq = void $ symbol (DText.pack "=")

newLineSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
newLineSpace = void $ takeWhile1P (Just "new line empty") (\ch -> ch /= '\n' && isSpace ch)

spaces :: ExprParser m ()
spaces = L.space newLineSpace lineComment blockComment

spaces' :: ExprParser m ()
spaces' = L.space space1 lineComment blockComment

lineComment :: ExprParser m ()
lineComment  = L.skipLineComment (DText.pack "--")

blockComment :: ExprParser m ()
blockComment = L.skipBlockComment (DText.pack "{-") (DText.pack "-}")

inBrackets :: ExprParser m a -> ExprParser m a
inBrackets = between (symbol $ DText.pack "(") (symbol $ DText.pack ")")


term :: ExprParser m Expr
term  =  try (inBrackets expr)
     <|> parseLit
     <|> parseVar
     <|> parseLet

parseLit :: ExprParser m Expr
parseLit = Lit <$> value

parseVar :: ExprParser m Expr
parseVar = Var <$> identifier

parseLet :: ExprParser m Expr
parseLet = inBrackets $ do
    reservedWord $ DText.pack "let"
    name <- identifier
    eq
    as   <- expr
    reservedWord $ DText.pack "in"
    ex   <- expr
    return $ Let name as ex

expr :: ExprParser m Expr
expr = makeExprParser term operators
  where
    operators = [ [ InfixL (Mul <$ DText.unpack <$> symbol (DText.pack "*"))
                  , InfixL (Div <$ DText.unpack <$> symbol (DText.pack "/"))
                  ]
                , [ InfixL (Add <$ DText.unpack <$> symbol (DText.pack "+"))
                  , InfixL (Sub <$ DText.unpack <$> symbol (DText.pack "-"))
                  ]
                ]