module Main where

import Control.Monad (replicateM)
import Control.Concurrent (threadDelay)
import Data.Char (ord)
import System.IO
import System.Random (randomRIO)
import System.Console.ANSI

getLastStdinChar :: IO (Maybe Char)
getLastStdinChar = do
    hasInput <- hReady stdin
    if hasInput
        then
            do
                currentChar <- getChar
                maybeLastChar <- getLastStdinChar

                return $ case maybeLastChar of
                            Nothing -> Just currentChar
                            _       -> maybeLastChar
        else
            return Nothing

getKey = getLastStdinChar -- getKey is a more descriptive name          

data PhysicalBody2D = PhysicalBody2D {
                        visual :: String
                        , row  :: Int
                        , col  :: Int
                    }

instance Show PhysicalBody2D where
    show (PhysicalBody2D visual row col) = show visual ++ " (" ++ show row ++ ", " ++ show col ++ ")"

rows = 40
columns = 80

spawnRock :: IO PhysicalBody2D
spawnRock = do
    col <- randomRIO (0, columns - 1)
    return (PhysicalBody2D "o" 0 col)

renderBody :: PhysicalBody2D -> IO ()
renderBody (PhysicalBody2D visual row col) = do
    setCursorPosition row col
    let color = if visual == "o" then Red else White
    setSGR [SetColor Foreground Vivid color]
    putStr visual
    hFlush stdout

getOccupiedSpace :: PhysicalBody2D -> [(Int, Int)]
getOccupiedSpace (PhysicalBody2D v r c) = map (\i -> (r, c + i)) [0..length v - 1]

areColliding :: PhysicalBody2D -> PhysicalBody2D -> Bool
areColliding body1 body2 = or $ (==) <$> getOccupiedSpace body1 <*> getOccupiedSpace body2

applyGravity :: PhysicalBody2D -> PhysicalBody2D
applyGravity (PhysicalBody2D visual row col) = PhysicalBody2D visual (row + 1) col

areOnSameRow :: PhysicalBody2D -> PhysicalBody2D -> Bool
areOnSameRow body1 body2 = row body1 == row body2

isAboveGround :: PhysicalBody2D -> Bool
isAboveGround body = row body <= rows

printScore :: Int -> IO ()
printScore score = do
    setCursorPosition 0 columns
    print $ "Score: " ++ show score

gameLoop :: PhysicalBody2D -> [PhysicalBody2D] -> Int -> IO ()
gameLoop player@(PhysicalBody2D v r c) rocks score = do
    threadDelay 200000

    -- spawn between 0 and 4 rocks at random positions
    newRocksCount <- randomRIO (0, 4)
    newRocks <- replicateM newRocksCount spawnRock

    -- move all rocks
    let allRocks = newRocks ++ filter isAboveGround rocks
    let movedRocks = map applyGravity allRocks

    -- move player - 'a' is left, 'd' is right, everything else doesnt move the player
    userKey <- getKey
    let movedPlayer = case userKey of
                        (Just 'a') -> PhysicalBody2D v r (max 0 (c - 1))
                        (Just 'd') -> PhysicalBody2D v r (min (columns - 1) (c + 1))
                        _   -> player


    -- calculate new score - for every rock that the player avoids, he gets a point
    let newScore = length rocks + newRocksCount - length allRocks + score

    -- print player and rocks
    clearScreen
    mapM_ renderBody (movedPlayer:movedRocks)
    printScore newScore

    -- check for collisions and if no collisions, enter next loop
    let collisionCandidates = filter (areOnSameRow player) movedRocks
    let isPlayerDead = any (areColliding movedPlayer) collisionCandidates
    if isPlayerDead then clearScreen >> print "loser" else gameLoop movedPlayer movedRocks newScore
    

main = do
    hideCursor
    hSetBuffering stdin NoBuffering -- dont wait for enter
    hSetEcho stdin False -- dont print user input

    setSGR [SetColor Background Vivid Black]
    setSGR [SetColor Foreground Vivid White]

    gameLoop (PhysicalBody2D "o_0" rows (columns `div` 2)) [] 0
