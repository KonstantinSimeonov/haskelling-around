main = do
    [n, xs, ws] <- mapM (const $ map read . words <$> getLine) [1..3]
    print . round1 $ weightedMean xs ws
    where
        round1 = (/ 10) . fromIntegral . round . (* 10)
        weightedMean xs ws = fromIntegral (sum $ zipWith (*) xs ws) / fromIntegral (sum ws)
