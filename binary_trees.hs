-- Binary Tree representation
data BTree a = Fork (BTree a) a (BTree a) | Empty deriving (Eq, Show)


-- Insert an element into a Binary Tree
insert :: Ord a => a -> BTree [a] -> BTree [a]
insert x ys | ys == Empty           = Fork Empty [x] Empty
            | x == head(q)          = Fork ps (x:q) rs
            | x < head(q)           = Fork (insert x ps) q rs
            | x > head(q)           = Fork ps q (insert x rs)
                where Fork ps q rs = ys


-- Flatten a Binary Tree into a list
flatten :: BTree [a] -> [a]
flatten Empty        = []
flatten (Fork ps q rs) = (flatten ps) ++ q ++ (flatten rs)


-- Insertion sort
bsort :: Ord a => [a] -> [a]
bsort = flatten.foldr insert Empty


main = do
   print (insert 9. insert 4 . insert 6 $ Empty)  -- f (g x) = f $ g x
   print ((insert 9 . insert 4 . insert 6) Empty)
   print (flatten (Fork (Fork Empty [4] Empty) [6] (Fork Empty [9] Empty)))
   print (bsort [10,6,6,2,9,3,1,2])

{- Output:
      Fork (Fork Empty [4] Empty) [6] (Fork Empty [9] Empty)
      Fork (Fork Empty [4] Empty) [6] (Fork Empty [9] Empty)
      [4,6,9]
      [1,2,2,3,6,6,9,10]
-}
