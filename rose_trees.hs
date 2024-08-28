{- Rose Trees are constructed from nodes and lists of their (unbounded no. of) children
   e.g.     1 "a"     =   RTree 1 "a" [RTree 2 "b" [RTree 5 "e" []], RTree 3 "c" [], RTree 4 "d" []]
           /  |  \
      2 "b"  3 "c"  4 "d"
       /
     5 "e"                                                                                        -}
data RTree a b = RTree a b [RTree a b] deriving (Eq, Show)


dfs :: Eq a => a -> RTree a b -> [b]
dfs x (RTree p q rs) | p == x     = concat ([q] : (map (dfs x) rs))
                     | otherwise  = concat (map (dfs x) rs)

{- Note:    concat ([q] : (map (dfs x) rs))   is used rather than   [q] ++ concat (map (dfs x) rs)
            as (++) has quadratic complexity, while (:) has linear complexity.

            (++) :: [a] -> [a] -> [a]
            (++) [] ys = ys
            (++) (x:xs) ys = x : (xs ++ ys)             -- traverses xs each iteration, so O(n^2)
-}


bfs :: Eq a => a -> RTree a b -> [b]
bfs x (RTree p q rs) = f x [RTree p q rs] []
        where f x [] ys = reverse ys
              f x ((RTree p q rs):ts) ys | p == x     = f x (ts ++ rs) (q:ys)
                                         | otherwise  = f x (ts ++ rs) ys


main = do
        let tree = RTree "apple" "red" [RTree "pear" "green" [RTree "apple" "round" [], RTree "banana" "soft" []], RTree "apple" "crunchy" [], RTree "banana" "yellow" [RTree "plum" "sweet" [], RTree "apple" "delicious" []]]

        print (dfs "apple" tree)
        print (bfs "apple" tree)

        print (dfs "banana" tree)
        print (bfs "banana" tree)

{- Output:
      ["red","round","crunchy","delicious"]
      ["red","crunchy","round","delicious"]
      ["soft","yellow"]
      ["yellow","soft"]
-}
