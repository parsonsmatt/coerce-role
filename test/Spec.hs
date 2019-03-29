{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DerivingStrategies #-}

module Main where

import UnliftIO

import CoerceRole

newtype MyIO m a = MyIO (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (MonadUnliftIO m, CoerceRole m) => MonadUnliftIO (MyIO m)

main :: IO ()
main = putStrLn "This module is a demonstration on how to use this library."
