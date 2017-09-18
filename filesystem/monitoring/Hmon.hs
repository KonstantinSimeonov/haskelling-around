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

runCmd :: String -> FilePath -> IO ()
runCmd command path = callProcess command [path]

ignoreError :: IO () -> IO ()
ignoreError x = catch x ignore
    where
        ignore :: SomeException -> IO ()
        ignore _ = return ()

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    let command  = args !! 1
    fullPath <- canonicalizePath filePath
    putStrLn $ "Watching file " ++ fullPath
    ignoreError $ runCmd command fullPath
    lastTimeModified <- getModificationTime fullPath
    setInterval 1000 (\time -> do
            newTime <- getModificationTime filePath
            if time < newTime
                then ignoreError $ runCmd command fullPath
                else return () :: IO ()
            return newTime) lastTimeModified