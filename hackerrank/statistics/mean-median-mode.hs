module Main where

import qualified Data.Map as M
import Data.List

main = do
    n <- read <$> getLine :: IO Int
    ns <- map (fromIntegral . read) . words <$> getContents :: IO [Double]
    let mean = sum ns / fromIntegral n
        half = drop (n `div` 2 - 1) $ sort ns
        ml = half !! 1
        mr = if n `mod` 2 /= 0 then ml else head half
        median = (ml + mr) / 2
        mode = fst $ maximumBy cmp $ M.toList $ foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty ns
    putStr . unlines . map show $ [mean, median, mode]
    where
        cmp (a1, a2) (b1, b2) = let c = compare a2 b2
                                in if c == EQ then compare b1 a1 else c
