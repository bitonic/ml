{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances #-}
module Fresh
       ( FreshT
       , runFreshT
       , evalFreshT
       , execFreshT
       , Fresh
       , runFresh
       , evalFresh
       , execFresh
       , MonadFresh (..)
       ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity

newtype FreshT c m a = FreshT {unFreshT :: StateT c m a}
                     deriving (Functor, Applicative, Monad, MonadTrans)

runFreshT :: (Enum c, Monad m) => FreshT c m a -> c -> m (a, c)
runFreshT = runStateT . unFreshT

evalFreshT :: (Enum c, Monad m) => FreshT c m a -> c -> m a
evalFreshT = evalStateT . unFreshT

execFreshT :: (Enum c, Monad m) => FreshT c m a -> c -> m c
execFreshT = execStateT . unFreshT

type Fresh c a = FreshT c Identity a

runFresh :: Enum c => Fresh c a -> c -> (a, c)
runFresh = runState . unFreshT

evalFresh :: Enum c => Fresh c a -> c -> a
evalFresh = evalState . unFreshT

execFresh :: Enum c => Fresh c a -> c -> c
execFresh = execState . unFreshT

class Monad m => MonadFresh c m | m -> c where
    fresh :: m c

instance (Enum c, Monad m) => MonadFresh c (FreshT c m) where
    fresh = FreshT (do {c <- get; modify succ; return c})

    reset = FreshT (do {put
