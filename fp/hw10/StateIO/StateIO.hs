-- (1 балл)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return x = StateIO $ const $ return x 
    (StateIO m) >>= k = StateIO bind'
        where
            bind' s = do
                x <- m s
                getStateIO (k x) s

instance MonadState s (StateIO s) where
    get = StateIO (\s -> readIORef s) 
    put x = StateIO (\s -> writeIORef s x)

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO m x = do
    r <- newIORef x
    a <- getStateIO m r
    s <- readIORef r
    return (a, s)

execStateIO :: StateIO s a -> s -> IO s
execStateIO m x = liftM snd $ runStateIO m x

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO m x = liftM fst $ runStateIO m x
