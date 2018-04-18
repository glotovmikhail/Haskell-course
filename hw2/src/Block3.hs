module Block3 where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad       (join, (>=>))
import Data.Bifunctor      (first)
import Data.List           (isPrefixOf)
import Data.Char           (isSpace)

--Task 1

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s)
  where
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s)
  where
    pure a                        = Parser $ \s -> Just (a, s)
    (<*>) (Parser pf) (Parser pa) = Parser $ pf >=> \(f, s) ->
                                       pa s >>= \(a, left) ->
                                       Just (f a, left)
instance Monad (Parser s)
  where
    return             = pure
    (>>=) (Parser p) f = Parser $ \s -> join (uncurry runParser <$> (first f <$> p s))

instance Alternative (Parser s)
  where
    empty = Parser (const Nothing)
    (<|>) (Parser pf) (Parser pa) = Parser $ \s -> pf s <|> pa s

-- Task 2

ok :: Parser a ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser a ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \s -> case s of
    []     -> Nothing
    (x:xs) -> if p x
              then Just (x, xs)
              else Nothing

element :: (Eq a) => a -> Parser a a
element c = satisfy(== c)

stream :: (Eq a) => [a] -> Parser a [a]
stream s = Parser $ \c -> if s `isPrefixOf` c
                          then Just (s, snd (splitAt (length s) c))
                          else Nothing

deleteSpaces :: Parser Char String
deleteSpaces = many (satisfy isSpace)

bracketsParse :: Parser Char String
bracketsParse = deleteSpaces *>
                (concat <$> many ((\l m r -> l:m ++ [r])
                        <$> element '(' <*> bracketsParse 
                                        <*> (element ')' <* deleteSpaces))) <* deleteSpaces

eofBracketsParse :: Parser Char String
eofBracketsParse = bracketsParse <* eof