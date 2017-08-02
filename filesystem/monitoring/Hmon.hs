#! /usr/bin/runhaskell

module Main where

import Control.Exception
import Control.Concurrent
import System.Directory
import System.Environment
import System.Process

setInterval :: Int -> (a -> IO a) -> a -> IO ()
setInterval ms action initial = do
    threadDelay $ ms * 1000
    actionResult <- action initial
    setInterval ms action actionResult
    
runhaskell :: FilePath -> IO ()
runhaskell path = callProcess "/usr/bin/runhaskell" [path]

ignoreError :: IO () -> IO ()
ignoreError x = catch x ignore
    where
        ignore :: SomeException -> IO ()
        ignore _ = return ()

main :: IO ()
main = do
    filePath <- fmap head getArgs
    fullPath <- canonicalizePath filePath
    putStrLn $ "Watching file " ++ fullPath
    putStrLn $ replicate 100 '*'
    ignoreError $ runhaskell fullPath
    lastTimeModified <- getModificationTime fullPath
    setInterval 1000 (
        \time -> do
            newTime <- getModificationTime filePath
            if time < newTime
                then ignoreError $ runhaskell fullPath
                else return () :: IO ()
            return newTime
        ) lastTimeModified