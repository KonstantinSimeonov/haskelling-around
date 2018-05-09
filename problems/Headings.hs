import Data.List

test1 = [ "H1 gosho1"
        , "H2 ivan"
        , "H3 penkaginka"
        , "H2 toshko"
        , "H1 ivancho"
        , "H3 stamatcho"
        , "H2 zdr"
        , "H2 kp"
        , "H4 gosho"
        , "H3 allo"
        , "H1 kek"
        ]

data TreeView a = Node a [TreeView a] deriving Show

showTree (Node (_, x) []) = x
showTree (Node (_, x) ns) = concat [ x
                                   , "<ol>"
                                   , foldl' (\acc li -> concat [acc, "<li>", li, "</li>"]) "" (map showTree $ reverse ns)
                                   , "</ol>"
                                   ]

parseHeader :: String -> (Int, String)
parseHeader header = let (priority, text) = break (== ' ') header
                     in (read $ tail priority, tail text)

solve :: [String] -> TreeView (Int, String)
solve headers = solve' headers [Node (0, "") []] where
    solve' [] ps = foldl1' link ps
    solve' (line:ls) parents = let currentHeading@(priority, text) = parseHeader line
                                   (heavier, lighter) = span (isLighter priority) parents -- remove heavier parents from the stack
                                   linkedParents = foldl1' link heavier -- link the heavier parents on the way up
                                   (Node heading children) : ps = lighter -- get the current parent
                                   newChildren = if null heavier then children else linkedParents : children -- add the linked parents if any
                                   newParents = [Node currentHeading [], Node heading newChildren] ++ ps -- add the new heading as the next parent
                               in solve' ls newParents
    link node (Node t ns) = Node t (node:ns) -- make the first argument a child of the second argument
    isLighter priority (Node (w, _) _) = w >= priority

main = putStrLn . showTree . solve $ test1
