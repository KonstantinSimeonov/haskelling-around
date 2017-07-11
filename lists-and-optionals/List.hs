module List where

data List a = Empty | a :-: List a

infixr 5 :-:

instance (Show a) => Show (List a) where
    show Empty = "{}"
    show xs    = "{ " ++ show' xs ++ " }"
        where
            show' Empty = ""
            show' (x :-: Empty) = show x
            show' (x :-: xs) = show x ++ ", " ++ show' xs

lrange :: (Enum a) => a -> a -> List a
lrange s e
    | fromEnum s == fromEnum e = Empty
    | otherwise = s :-: lrange (succ s) e

(...) :: (Enum a) => a -> a -> List a
(...) = lrange

infixr 5 ...

(+++) :: List a -> List a -> List a
Empty      +++ ys = ys
(x :-: xs) +++ ys = x :-: (xs +++ ys)

fromList :: [a] -> List a
fromList = foldr (:-:) Empty

instance Foldable List where
    foldr fn v0 Empty = v0
    foldr fn v0 (x :-: xs) = fn x (foldr fn v0 xs)

    foldr1 fn Empty = error "Cannot foldr1 on empty List"
    foldr1 fn (x :-: xs) = foldr fn x xs

instance Functor List where
    fmap fn Empty = Empty
    fmap fn (x :-: xs) = fn x :-: fmap fn xs

instance Applicative List where
    pure = (:-: Empty)
    fns <*> xs = foldr (+++) Empty $ fmap (\f -> fmap f xs) fns

instance Monad List where
    return = pure
    xs >>= fn = foldr (+++) Empty $ fmap fn xs

lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b

lseq :: Applicative f => List (f a) -> f (List a)
lseq = foldr (lift2 (:-:)) (pure Empty)
