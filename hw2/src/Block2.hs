{-# LANGUAGE InstanceSigs #-}

module Block2 where

import Data.Char (isDigit)

-- Task 1

isItNum :: String -> Bool
isItNum list@(x:list'@(x':xs)) = if (x == '-' && x' /= '-')
                                 then isItNum list'
                                 else all isDigit list 
isItNum list@(x:xs)            = if (x == '-')
                                 then isItNum xs
                                 else all isDigit list 
isItNum x                      = all isDigit x

getInt :: String -> Maybe Int
getInt s = if isItNum s
           then Just (read s)
           else Nothing 

stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse getInt (words s)

-- Task 2

newtype Optional a = Optional (Maybe (Maybe a))

instance Functor Optional 
  where
    fmap f (Optional (Just (Just x))) = Optional (Just (Just (f x)))
    fmap _ (Optional (Just Nothing))  = Optional (Just Nothing)
    fmap _ (Optional Nothing)         = Optional Nothing

instance Applicative Optional 
  where
    pure x                             = Optional (Just (Just x))
    (<*>) (Optional (Just (Just f))) x = f <$> x
    (<*>) _ (Optional (Just Nothing))  = Optional (Just Nothing)
    (<*>) _ (Optional Nothing)         = Optional Nothing
    (<*>) _ _                          = undefined

instance Monad Optional 
  where
    return x = Optional (Just (Just x))
    (>>=) (Optional (Just (Just x))) f = f x
    (>>=) (Optional (Just Nothing)) _  = Optional (Just Nothing)
    (>>=) (Optional Nothing) _         = Optional Nothing

instance Foldable Optional 
  where
    foldMap = optional mempty
      where
        optional :: b -> (a -> b) -> Optional a -> b
        optional _ f (Optional (Just (Just x))) = f x
        optional d _ _                          = d

instance Traversable Optional 
  where
    traverse f (Optional (Just (Just x))) = (Optional . Just . Just) <$> f x
    traverse _ (Optional (Just Nothing))  = pure (Optional (Just Nothing))
    traverse _ (Optional Nothing)         = pure (Optional Nothing)

{--
Monad Laws:
    1. Left Identity:
        return a >>= f ≡ f a
    2. Right Identity: 
        m >>= return ≡ m
    3. Associativity:
        (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

Proofs:
    1. return a >>= f ≡ Optional (Just (Just a)) >>= f                // return
                     ≡ f a                                            // bind

    2. m = Optional (Just (Just a))
       Optional (Just (Just a)) >>= return ≡ return a                 // bind
                                           ≡ Optional (Just (Just a)) // return

       m = Optional (Just Nothing)
       Optional (Just Nothing) >>= return ≡ Optional (Just Nothing)   // bind

       m = Optional Nothing
       Optional Nothing >>= return ≡ Optional Nothing                 // bind

    3. m = Optional (Just (Just a))
       (Optional (Just (Just a)) >>= f) >>= g 
           ≡ f (Optional (Just (Just a))) >>= g                       // bind f
       Optional (Just (Just a)) >>= (\x -> f x >>= g)
           ≡ (\x -> f x >>= g) (Optional (Just (Just a)))             // bind 
           ≡ f (Optional (Just (Just a))) >>= g                       // func apply

       m = Optional (Just Nothing)
       (Optional (Just Nothing)) >>= f) >>= g
           ≡ f (Optional (Just Nothing)) >>= g                        // bind
       Optional (Just Nothing) >>= (\x -> f x >>= g)_
           ≡ (\x -> f x >>= g) Optional (Just Nothing)                // bind
           ≡ f (Optional (Just Nothing)) >>= g                        // func apply

       m = Optional Nothing
       (Optional Nothing >>= f) >>= g
           ≡ f (Optional Nothing) >>= g                               // bind
       Optional Nothing >>= (\x -> f x >>= g)_
           ≡ (\x -> f x >>= g) Optional Nothing                       // bind
           ≡ f (Optional Nothing) >>= g                               // func apply
--}

-- Task 3

data NonEmpty a = a :| [a]

fromList :: [a] -> NonEmpty a
fromList (x:xs) = x :| xs
fromList _      = undefined

toList :: NonEmpty a -> [a]
toList (x:|xs) = x:xs

instance Functor NonEmpty 
  where
    fmap f (x :| xs) = f x :| map f xs

instance Applicative NonEmpty 
  where
    pure x = x :| []
    (<*>) (f:|fs) (x:|xs) = fromList [g y | g <- f:fs, y <- x:xs]

instance Monad NonEmpty 
  where
    return x = x :| []
    (>>=) (l:|ls) f = x :| (xs ++ ys)
                  where
                    x :| xs = f l
                    ys = ls >>= toList . f

instance Foldable NonEmpty 
  where
    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x :| xs) = f x `mappend` foldMap f xs

instance Traversable NonEmpty 
  where
    traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
