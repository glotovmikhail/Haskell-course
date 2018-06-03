{-# LANGUAGE Rank2Types #-}

module Lens
       ( Lens
       , Lens'
       , set
       , view
       , over
       , (.~)
       , (^.)
       , (%~)
       , _1
       , _2
       , lens
       , choosing
       , (<%~)
       , (<<%~)
       ) where

import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))

-- Basic Lenses

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s
set l val obj = runIdentity $ l (const $ Identity val) obj

view :: Lens' s a -> s -> a
view l obj = getConst $ l Const obj

over :: Lens' s a -> (a -> a) -> s -> s
over l fun obj = runIdentity $ l (Identity . fun) obj

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\y -> (y, x)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\y -> (x, y)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter fun obj = setter obj <$> fun (getter obj)

-- set
(.~) :: Lens s t a b -> b -> s -> t
l .~ b = runIdentity . l (const $ Identity b)

-- view
(^.) :: s -> Lens s t a b -> a
obj ^. l = getConst $ l Const obj

-- over
(%~) :: Lens s t a b -> (a -> b) -> s -> t
l %~ fun = runIdentity . l (Identity . fun)

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (^.l1) (^.l2))
                      (either (\s1 b -> Left  $ (l1.~b) s1)
                              (\s2 b -> Right $ (l2.~b) s2))

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l fun obj = (fun (obj ^. l), (l %~ fun) obj)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l fun obj = (obj ^. l, (l %~ fun) obj)