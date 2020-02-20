{-# LANGUAGE NoImplicitPrelude #-}

module StaxRomana.Memory (
   module StaxRomana.Memory, module StaxRomana.Roman
) where

import Prelude hiding (head)
import StaxRomana.Roman


type Memory = Stack Roman
type Head = Roman
data Stack a = Empty | Stack a :< a


instance Show a => Show (Stack a) where
   show = show . toList

instance Functor Stack where
   fmap _ Empty  = Empty
   fmap f (s:<x) = fmap f s :< f x

instance Foldable Stack where
   foldMap f Empty  = mempty
   foldMap f (s:<x) = foldMap f s <> f x

instance Applicative Stack where
   pure = (Empty :<)
   fs <*> xs = fs >>= \f -> xs >>= return . f
   
instance Monad Stack where
   xs >>= f = foldMap f xs
   
instance Semigroup (Stack a) where
   xs <> Empty = xs
   xs <> (ys :< y) = (xs <> ys) :< y

instance Monoid (Stack a) where
   mempty = Empty


toList :: Stack a -> [a]
toList Empty  = []
toList (s:<x) = toList s ++ [x]

fromList :: [a] -> Stack a
fromList xs = pushn xs Empty

pop :: Stack a -> (a, Stack a)
pop Empty  = errorWithoutStackTrace "Cannot pop an empty stack!"
pop (s:<x) = (x, s)

popn :: Int -> Stack a -> ([a], Stack a)
popn 0 s = ([], s)
popn n s = let
   (xs, s') = popn (n-1) s
   (x, s'') = pop s'
   in (x:xs, s'')

push :: a -> Stack a -> Stack a
push = flip (:<)

pushn :: [a] -> Stack a -> Stack a
pushn [] s = s
pushn (x:xs) s = pushn xs $ push x s

head :: Memory -> Head
head Empty = 0
head (_:<x) = x

