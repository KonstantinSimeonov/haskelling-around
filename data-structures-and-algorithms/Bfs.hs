module Bfs where

import Queue

data TreeNode a = TreeNode {
                              value    :: a
                            , children :: [TreeNode a]
                        } deriving Show

bfs :: TreeNode a -> [a]
bfs tree = bfs' (Queue [tree] [])
    where
        bfs' :: Queue (TreeNode a) -> [a]
        bfs' (Queue [] []) = []
        bfs' que = value node:bfs' nodeChildren
            where
                (node, newQueue) = deq que
                nodeChildren = enqMany (children node) newQueue

sample = TreeNode 1 [ 
                        TreeNode 2 [ TreeNode 4 [], TreeNode 5 [TreeNode 8 []] ]
                        , TreeNode 3 [ TreeNode 6 [], TreeNode 7 [] ] 
                    ]

main = print $ bfs sample