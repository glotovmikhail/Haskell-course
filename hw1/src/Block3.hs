{-# LANGUAGE InstanceSigs #-}

module Block3 where

import Data.List          (iterate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Maybe         (isNothing)
import Data.Monoid        (Monoid (..))


--days

data Day = MON | TUE | WED | THU | FRI | SAT | SUN
    deriving (Eq, Enum, Show)

day :: [Day]
day = [MON, TUE, WED, THU, FRI, SAT, SUN]

nextDay :: Day -> Day
nextDay d
    | d == SUN  = MON
    | otherwise = succ d

afterDays :: Day -> Int -> Day
afterDays d n = iterate nextDay d !! (mod n 7)

isWeekend :: Day -> Bool
isWeekend SAT = True
isWeekend SUN = True
isWeekend _   = False

daysToParty :: Day -> Int
daysToParty FRI = 0
daysToParty d   = 1 + (daysToParty $ nextDay d)

-- castles

data Building = Library | Church
    deriving (Show)

data Family = One | Two | Three | Four
    deriving (Show, Enum)

newtype Man = Man
    { name :: String
    } deriving (Eq, Show)

newtype House = House Family
    deriving (Show)

newtype Castle = Castle
    { lordC :: Maybe Man
    } deriving (Show)

newtype Walls = Walls
    { lengthW :: Int
    } deriving (Show, Eq)

data ProtectBuilds = ProtectBuilds
    { castleC :: Castle
    , wallsC  :: Maybe Walls
    } deriving (Show)

data City = City
    { protectBuildC :: Maybe ProtectBuilds
    , buildingC     :: Maybe Building
    , housesC       :: NonEmpty House
    } deriving (Show)

data NewLordFail = NoCastle | AlreadyLord
    deriving (Eq, Show)

buildCastle :: City -> Castle -> (Bool, City)
buildCastle (City Nothing building houses) castle = (True
                                                    , City (Just (ProtectBuilds castle Nothing))
                                                    building houses)
buildCastle x _                                   = (False, x)

buildChrOrLib :: City -> Building -> (Bool, City)
buildChrOrLib (City protectBuild Nothing houses) building =
              (True, City protectBuild (Just building) houses)
buildChrOrLib x _ = (False, x)

buildHouse :: City -> Family -> City
buildHouse (City protectBuild building houses) family =
           City protectBuild building (House family<|houses)

countPeople :: City -> Int
countPeople (City _ _ houses) = foldl (\x (House family) -> x + fromEnum family + 1) 0 houses

buildWalls :: City -> Walls -> (Bool, City)
buildWalls city@(City (Just (ProtectBuilds (Castle (Just lord)) Nothing)) building houses) walls
           | countPeople city >= 10 = (True, City (Just (ProtectBuilds (Castle (Just lord)) 
                                                                       (Just walls)))
                                  building houses)
           | otherwise              = (False, city)
buildWalls city _ = (False, city)

meetLord :: City -> Man -> (City, Maybe NewLordFail)
meetLord city@(City Nothing _ _) _ = (city, Just NoCastle)
meetLord city@(City (Just (ProtectBuilds (Castle lord) walls)) building houses) nLord
         | isNothing lord = (City (Just (ProtectBuilds (Castle (Just nLord)) walls))
                                   building houses, Nothing)
         | otherwise = (city, Just AlreadyLord)





-- task 3 nat
data Nat = Z | S Nat
    deriving (Show, Eq)

natFromInt :: Int -> Nat
natFromInt 0 = Z
natFromInt n = S (natFromInt (n - 1))

intFromNat :: Nat -> Int
intFromNat Z     = 0
intFromNat (S n) = 1 + (intFromNat n)

add :: Nat -> Nat -> Nat
add Z b     = b
add a Z     = a
add a (S b) = S (add a b)

sub :: Nat -> Nat -> Nat
sub Z (S _)     = undefined
sub a Z         = a
sub (S a) (S b) = sub a b

mul :: Nat -> Nat -> Nat
mul _ Z     = Z
mul Z _     = Z
mul a (S b) = add a (mul a b)

instance Ord Nat
  where
    compare Z Z         = EQ
    compare (S _) Z     = GT
    compare Z (S _)     = LT
    compare (S a) (S b) = compare a b

-- hard version

isPrime :: Nat -> Bool
isPrime Z         = True
isPrime (S Z)     = False
isPrime (S (S a)) = isPrime a

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "Division by Zero"
divNat Z _ = Z
divNat a b
    | a == b    = (S Z)
    | a < b     = Z
    | otherwise = (S (divNat (sub a b) b))

modDivNat :: Nat -> Nat -> Nat
modDivNat _ Z = error "Division by Zero"
modDivNat Z a = a
modDivNat a b = sub a (mul (divNat a b) b)

-- BSTree

data BSTree a = Nil
              | Node [a] (BSTree a) (BSTree a)
    deriving (Eq, Show)

isEmpty :: BSTree a -> Bool
isEmpty Nil = True
isEmpty _   = False

size :: (Ord a) => BSTree a -> Int
size (Node lst left right) = size left + length lst + size right
size Nil                   = 0

contains :: (Show a, Ord a) => BSTree a -> a -> Bool
contains Nil _           = False
contains (Node (x:_) left right) k
                             | x > k  = contains left k
                             | x < k  = contains right k
                             | x == k = True
contains (Node [] _ _) _ = False
contains _ _             = undefined

addN :: (Ord a) => BSTree a -> a -> BSTree a
addN Nil x = Node [x] Nil Nil
addN (Node lst@(x:_) left right) k
                             | x > k  = Node lst (addN left k) right
                             | x < k  = Node lst left (addN right k)
                             | x == k = Node (k:lst) left right
addN _ _   = undefined

removeLeft :: (Ord a) => BSTree a -> ([a], BSTree a)
removeLeft (Node lst Nil right) = (lst, right)
removeLeft (Node l left r)      =
    let (ret, newLeft) = removeLeft left
    in
        (ret, Node l newLeft r)
removeLeft _                    = undefined

remove :: (Ord a) => BSTree a -> a -> BSTree a
remove Nil _ = Nil
remove (Node lst@(x:xs) left right) k
                             | x > k = Node lst (remove left k) right
                             | x < k = Node lst left (remove right k)
                             | x == k && length lst > 1 = Node xs left right
                             | x == k && right == Nil && left == Nil = Nil
                             | x == k && right == Nil = left
                             | x == k = Node newLst left newRight
                                    where
                                      (newLst, newRight) = removeLeft right
remove _ _   = undefined

fromList :: (Ord a) => [a] -> BSTree a
fromList = foldl addN Nil

instance Foldable BSTree 
  where
    foldMap :: Monoid m => (a -> m) -> BSTree a -> m
    foldMap _ Nil                   = mempty
    foldMap f (Node lst left right) = foldMap f left `mappend`
                                      foldMap f lst `mappend`
                                      foldMap f right
