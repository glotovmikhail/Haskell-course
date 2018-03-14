module Block5 where

import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..), Semigroup (..))
import Block4 (NonEmpty ((:|)))
import Data.Either   (partitionEithers)
import Data.Maybe    (fromMaybe)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat s = fromMaybe [] $ mconcat s

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat s = mconcatPair $ partitionEithers s
  where
    mconcatPair :: (Monoid a, Monoid b) => ([a], [b]) -> (a, b)
    mconcatPair (a, b) = (mconcat a, mconcat b)

instance Semigroup (NonEmpty t) where
    (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y:ys))

data ThisOrThat a b = This a 
                    | That b 
                    | Both a b 
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) 
  where
    (<>) (This a) (This b)     = This (a <> b)
    (<>) (That a) (That b)     = That (a <> b)
    (<>) (This a) (That b)     = Both a b
    (<>) (Both a b) (This c)   = Both (a <> c) b
    (<>) (Both a b) (That c)   = Both a (b <> c)
    (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)
    (<>) (This c) (Both a b)   = Both (c <> a) b
    (<>) (That c) (Both a b)   = Both a (c <> b)
    (<>) x y                   = y <> x

data Builder = One Char 
             | Many [Builder] 
    deriving (Show, Eq)

instance Semigroup Builder 
  where
    (<>) x (Many [])         = x
    (<>) (Many []) y         = y
    (<>) a@(One _) b@(One _) = Many [a, b]
    (<>) (Many xs) (Many ys) = Many (xs ++ ys)
    (<>) a@(One _) (Many ys) = Many $ a:ys
    (<>) (Many xs) b@(One _) = Many (xs ++ [b])

instance Monoid Builder 
  where
    mempty = Many []
    mappend = (<>)

fromString :: String -> Builder
fromString = foldr (mappend . One) mempty

toString :: Builder -> String
toString (One chr)   = [chr]
toString (Many chrs) = foldr (mappend . toString) mempty chrs