import Data.Functor
import Control.Applicative
import Control.Monad

newtype St stateT resultT = St { runState :: stateT -> (resultT, stateT) }

-- feed result of a stateful computation to a function
instance Functor (St stateT) where
    fmap fn stateF = St $ \x -> let (result, state) = runState stateF x
                                in (fn result, state)

-- apply a function that is the result of a stateful
-- computation to a result of a stateful computation
instance Applicative (St stateT) where
    pure x = St $ \state -> (x, state)
    sta1 <*> sta2 = St $ \x -> let (resultFn, state) = runState sta1 x
                               in runState (resultFn <$> sta2) state

-- chain stateful computations
instance Monad (St s) where
    stm >>= fn = St $ \x -> let (result, state) = runState stm x
                            in runState (fn result) state

get :: St stateT stateT
get = St (\x -> (x, x))

put :: stateT -> St stateT ()
put x = St (\_ -> ((), x))

test :: St Int whatever -> St Int String
test st = do
    x <- get
    put $ x + 5
    newX <- get
    pure . show $ newX * 2

main = do
    x <- readLn
    let result = runState (test $ pure ()) x
    print result
