{-# LANGUAGE GADTs #-}

import Data.Maybe
import Data.List (intercalate)
import System.IO (writeFile)
import System.Random (randomRIO)
import Control.Monad (forM_)

data BinaryTree a = Ord a => BinaryTree a (BinaryTree a) (BinaryTree a) | Empty

insert :: Ord a => BinaryTree a -> a -> BinaryTree a
insert Empty x = BinaryTree x Empty Empty
insert tree@(BinaryTree v left right) x
  | v > x     = BinaryTree v (insert left x) right
  | v < x     = BinaryTree v left (insert right x)
  | otherwise = tree

remove :: Ord a => BinaryTree a -> a -> BinaryTree a
remove Empty _ = Empty
remove node@(BinaryTree v left right) x
  | v > x     = BinaryTree v (remove left x) right
  | v < x     = BinaryTree v left (remove right x)
  | otherwise = successor node
  where
    successor :: BinaryTree a -> BinaryTree a
    successor Empty = Empty
    successor (BinaryTree _ Empty Empty) = Empty
    successor (BinaryTree _ Empty right) =
      let (r, t) = left' right
      in BinaryTree r Empty t
    successor (BinaryTree _ left right) =
      let (r, t) = right' left
      in BinaryTree r t right

    right' :: BinaryTree a -> (a, BinaryTree a)
    right' (BinaryTree v Empty Empty) = (v, Empty)
    right' (BinaryTree v Empty right) = let (r, t) = right' right
                                        in (r, BinaryTree v Empty t)
    right' (BinaryTree v left right) = let (r, t) = right' left
                                       in (r, BinaryTree v t right)

    left' :: BinaryTree a -> (a, BinaryTree a)
    left' (BinaryTree v Empty Empty) = (v, Empty)
    left' (BinaryTree v left Empty) = let (r, t) = left' left
                                      in (r, BinaryTree v t Empty)
    left' (BinaryTree v left right) = let (r, t) = left' right
                                      in (r, BinaryTree v left t)

contains :: Ord a => BinaryTree a -> a -> Bool
contains Empty _ = False
contains (BinaryTree v l r) x
  | x > v     = contains r x
  | x < v     = contains l x
  | otherwise = True

value :: BinaryTree a -> Maybe a
value Empty = Nothing
value (BinaryTree x _ _) = Just x

nodes :: BinaryTree a -> [BinaryTree a]
nodes Empty = []
nodes node@(BinaryTree _ left right) = (nodes left) ++ [node] ++ (nodes right)

toDot :: Show a => String -> BinaryTree a -> String
toDot name t = "digraph " ++ name ++ " {\n" ++ (intercalate "\n" edges) ++ "\n}"
  where
    mkEdge :: Show a => a -> BinaryTree a -> String
    mkEdge v node = "    \"" ++ show v ++ "\" -> \"" ++ to ++ "\";"
      where
        to = case node of
          Empty            -> show v ++ "null"
          BinaryTree c _ _ -> show c

    edges = concatMap (\(BinaryTree v l r) -> [mkEdge v l, mkEdge v r]) . nodes $ t

toList :: BinaryTree a -> [a]
toList = map (\(BinaryTree x _ _) -> x) . nodes

main = do
  --xs <- mapM (const $ randomRIO (0, 40)) [1..20] :: IO [Int]
  let xs = [4,3,20,7,17,24,20,39,2,8,1,33,36,35,2,19,40,23,5,14] :: [Int]
  let tree = foldl insert Empty xs
  print . toList $ tree
  putStrLn . toDot "" $ tree
  writeFile "tree.dot" $ toDot "" tree
  forM_ ([33, 39, 7, 17, 3, 1] :: [Int]) (\x -> writeFile ("tree" ++ show x ++ ".dot") . toDot ("\"rm " ++ show x ++ "\"") $ remove tree x)
