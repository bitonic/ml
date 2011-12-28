{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Fresh
       ( MonadFresh (..)
       , FreshT
       , runFreshT
       , evalFreshT
       , execFreshT
       , mapFreshT
       , Fresh
       , runFresh
       , evalFresh
       , execFresh
       ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

class Monad m => MonadFresh c m | m -> c where
    fresh :: Monad m => m c

instance (Error e, MonadFresh c m) => MonadFresh c (ErrorT e m) where
    fresh = lift fresh

instance MonadFresh c m => MonadFresh c (StateT s m) where
    fresh = lift fresh

newtype FreshT c m a = FreshT {unFreshT :: StateT c m a}
                     deriving (Functor, Applicative, Monad, MonadTrans)

instance (Enum c, Monad m) => MonadFresh c (FreshT c m) where
    fresh = FreshT (do {c <- get; modify succ; return c})

instance MonadError e m => MonadError e (FreshT c m) where
    throwError = lift . throwError
    catchError (FreshT m) h =
        FreshT . StateT $
        \s -> runStateT m s `catchError` \e -> runStateT (unFreshT $ h e) s

instance MonadState s m => MonadState s (FreshT c m) where
    get = lift get
    put = lift . put

runFreshT :: (Enum c, Monad m) => FreshT c m a -> c -> m (a, c)
runFreshT m = runStateT (unFreshT m)

evalFreshT :: (Enum c, Monad m) => FreshT c m a -> c -> m a
evalFreshT m = evalStateT (unFreshT m)

execFreshT :: (Enum c, Monad m) => FreshT c m a -> c -> m c
execFreshT m = execStateT (unFreshT m)

mapFreshT :: Enum c => (m (a, c) -> n (b, c)) -> FreshT c m a -> FreshT c n b
mapFreshT f = FreshT . mapStateT f . unFreshT

type Fresh c = FreshT c Identity

runFresh :: Enum c => Fresh c a -> c -> (a, c)
runFresh m = runState (unFreshT m)

evalFresh :: Enum c => Fresh c a -> c -> a
evalFresh m = evalState (unFreshT m)

execFresh :: Enum c => Fresh c a -> c -> c
execFresh m = execState (unFreshT m)
