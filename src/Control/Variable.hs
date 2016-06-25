{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Variable
    ( VVar
    , Late
    , actor
    , actOn
    ) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM

type Late = Maybe

data V s t where
    Pure :: a -> V s a
    Ap :: V s (a -> b) -> V s a -> V s b
    Var :: s a -> V s (Late a)

newtype Actor a = Actor { act :: ((a -> IO ()) -> IO ()) }

newtype MTVar a = MTVar (TVar (Late a))

instance Functor (V s) where
    fmap = Ap . Pure

instance Applicative (V s) where
    pure = Pure
    (<*>) = Ap

eval :: V MTVar t -> STM t
eval (Pure a) = pure a
eval (Ap f x) = eval f <*> eval x
eval (Var (MTVar v)) = readTVar v

-- Monadic recursion, so that result of 'go' is in scope?
compile :: V Actor t -> IO (V MTVar t, MVar ())
compile var = do
    changed <- newEmptyMVar
    let go :: V Actor t -> IO (V MTVar t)
        go (Pure a) = return $ Pure a
        go (Ap x y) = Ap <$> go x <*> go y
        go (Var (Actor f)) = do
            tvar <- atomically $ newTVar Nothing
            forkIO . f $ \x -> void $ do
                atomically $ writeTVar tvar (Just x)
                tryPutMVar changed ()
            return $ Var (MTVar tvar)
    (, changed) <$> go var

actOn :: VVar t -> (t -> IO ()) -> IO ()
actOn var action = do
    (fresh, changed) <- compile var
    forever $ do
        atomically (eval fresh) >>= action
        catch (takeMVar changed) $ \(e :: SomeException) -> threadDelay maxBound

type VVar = V Actor

actor :: ((a -> IO ()) -> IO ()) -> VVar (Late a)
actor = Var . Actor
