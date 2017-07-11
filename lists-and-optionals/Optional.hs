module Optional where

data Optional a = Value a | None deriving Show

instance Functor Optional where
    fmap _ None = None
    fmap fn (Value x) = Value $ fn x

instance Applicative Optional where
    pure = Value
    None <*> _ = None
    (Value fn) <*> ox = fmap fn ox

instance Monad Optional where
    return = pure
    (Value x) >>= fn = fn x
    None >>= _ = None

-- safe integer division using Optionals
(//) :: (Eq a, Integral a) => Optional a -> Optional a -> Optional a
_ // (Value 0)         = None
-- ox // oy = ox >>= \x -> oy >>= \y -> return $ x `div` y
ox // oy = do
        x <- ox
        y <- oy
        return $ x `div` y
