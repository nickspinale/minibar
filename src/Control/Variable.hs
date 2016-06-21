{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Control.Variable
    ( VVar
    , actor
    , actOn
    ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM

data V s t where
    Pure :: a -> V s a
    Ap :: V s (a -> b) -> V s a -> V s b
    Var :: s a -> V s a

data Actor a = Actor a ((a -> IO ()) -> IO ())

instance Functor (V s) where
    fmap = Ap . Pure

instance Applicative (V s) where
    pure = Pure
    (<*>) = Ap

eval :: V TVar t -> STM t
eval (Pure a) = pure a
eval (Ap f x) = eval f <*> eval x
eval (Var  v) = readTVar v

-- Monadic recursion, so that result of 'go' is in scope?
compile :: V Actor t -> IO (V TVar t, MVar ())
compile var = do
    changed <- newEmptyMVar
    let go :: V Actor t -> IO (V TVar t)
        go (Pure a) = return $ Pure a
        go (Ap x y) = Ap <$> go x <*> go y
        go (Var (Actor x0 f)) = do
            tvar <- atomically $ newTVar x0
            forkIO . f $ \x -> void $ do
                atomically $ writeTVar tvar x
                tryPutMVar changed ()
            return $ Var tvar
    (, changed) <$> go var

actOn :: V Actor t -> (t -> IO ()) -> IO ()
actOn var action = do
    (fresh, changed) <- compile var
    forever $ do
        takeMVar changed
        atomically (eval fresh) >>= action

type VVar = V Actor

actor :: a -> ((a -> IO ()) -> IO ()) -> VVar a
actor = ((.).(.)) Var Actor
