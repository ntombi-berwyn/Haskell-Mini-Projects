-- Singleton can be represented as 'Singleton a' or 'Union (Singleton a) (Empty)'
data Set a = Empty | Singleton a | Union (Set a) (Set a) deriving Show


-- Deconstructor
set :: b -> (a -> b) -> (Set a -> Set a -> b) -> Set a -> b
set empty singleton union = f
        where f Empty = empty
              f (Singleton x) = singleton x
              f (Union p q) = union p q


-- Fold on sets
foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet empty singleton union = g
        where g Empty = empty
              g (Singleton x) = singleton x
              g (Union p q) = union (g p) (g q)


-- Tests if an element is in a set
isIn :: Eq a => a -> Set a -> Bool
isIn y = h
        where h Empty = False
              h (Singleton x) = x == y
              h (Union p q) = h p || h q

isIn' :: Eq a => a -> Set a -> Bool
isIn' y = foldSet False (==y) (||)


-- Find if a set (set1) is a subset of another (set2)
-- To find if set1 a superset of set2, switch set1 and set2 parameters in subset
subset :: Eq a => Set a -> Set a -> Bool
subset Empty set = True
subset (Singleton x) set = isIn' x set
subset (Union p q) set = subset p set && subset q set

subset' :: Eq a => Set a -> Set a -> Bool
subset' set1 set2 = foldSet True (\x -> isIn' x set2) (&&) set1


-- Another, slightly worse way to do subset (involving lists)
list' :: Set a -> [a]
list' = foldSet [] (\x -> [x]) (++)

subset'' :: Eq a => Set a -> Set a -> Bool
subset'' set1 set2 = foldr (&&) True (map f (list' set1))
        where f x = isIn' x set2


-- Tests if two sets are equal
equal :: Eq a => Set a -> Set a -> Bool
equal set1 set2 = subset set1 set2 && subset set2 set1


-- Finds if two sets are disjoint (Empty set dijoint from itself)
disjoint :: Eq a => Set a -> Set a -> Bool
disjoint set1 set2 = not (foldr (||) False (map f (list' set1)))
        where f x = isIn' x set2


{- Some issues while testing with setC and setD, which both represent the empty set
        Ambiguous type variable ‘a0’ arising from a use of ‘disjoint’
        prevents the constraint ‘(Eq a0)’ from being solved.                      -}
main = do
  let setA = Union (Singleton 1) (Union (Singleton 2) (Singleton 3))
  let setB = Union (Singleton 2) (Singleton 1)
  let setC = Empty
  let setD = Union (Empty) (Empty)
  print(isIn 1 setA)    -- True
  print(isIn' 1 setA)   -- True
  print(isIn' 4 setA)   -- False
  print(isIn' 4 setC)   -- False
  print(subset setB setA)   -- True
  print(subset' setB setA)  -- True
  print(subset'' setB setA) -- True
  print(subset' setA setB)  -- False
  --print((subset' setC setA) && (subset' setC setB) && (subset' setC setD))  -- True
  --print(subset' setD setD)  -- True
  print(equal setA setA)  -- True
  --print(equal setC setD)  -- True
  print(equal setA setD)  -- False
  print(disjoint setA setA) -- False
  print(disjoint setA setB)  -- False
  print(disjoint setA setC)  -- True
  --print(disjoint setD setD) -- True
  --print(disjoint setC setC) -- True
