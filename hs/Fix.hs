{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances #-}
module Fix
       ( Fix (..)
       , rollF
       , unRollF
       ) where

newtype Fix f = Roll {unRoll :: f (Fix f)}

instance (Show (f (Fix f))) => Show (Fix f) where
    show (Roll f) = show f

instance (Eq (f (Fix f))) => Eq (Fix f) where
    Roll l == Roll r = l == r

rollF :: (forall a. f a -> b) -> Fix f -> b
rollF f d = f (unRoll d)

unRollF :: (Fix f -> b) -> f (Fix f) -> b
unRollF f d = f (Roll d)
