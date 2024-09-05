-- Matrix representation, assumed to be rectangular and non-empty
type Matrix a = [[a]]


-- Transpose of a matrix (1st definition is not a fold on lists)
cols :: [[a]] -> [[a]]
cols [xs] = [ [x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

cols' :: [[a]] -> [[a]]
cols' [] = repeat []
cols' (xs:xss) = zipWith (:) xs (cols' xss)

cols'' :: [[a]] -> [[a]]
cols'' = foldr (zipWith (:)) (repeat [])

cols''' :: [[a]] -> [[a]]
cols''' = foldr (\xs xss -> zipWith (:) xs xss) (repeat [])


-- Scalar multiplication by n (by recursion and partial function application)
scale :: Num a => a -> Matrix a -> Matrix a
scale n [] = []
scale n (xs:xss) = map (n*) xs : scale n xss

scale' :: Num a => a -> Matrix a -> Matrix a
scale' n xs = map (map (n*)) xs


-- Dot product of two same-length vectors
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

dot' :: Num a => [a] -> [a] -> a
dot' xs ys = (sum.zipWith (*) xs) ys


-- Matrix addition
add :: Num a => Matrix a -> Matrix a -> Matrix a
add xss yss = zipWith (zipWith (+)) xss yss


-- Matrix multiplication
mul :: Num a => Matrix a -> Matrix a -> [a]
mul xss yss = map sum zs
   where zs = [zipWith (*) xs ys | xs <- xss, ys <- cols yss]

mul' :: Num a => Matrix a -> Matrix a -> [a]
mul' xss yss = [dot xs ys | xs <- xss, ys <- cols yss]

mul'' :: Num a => Matrix a -> Matrix a -> Matrix a
mul'' [] _ = []
mul'' (x:xs) yss = map (dot x) (cols yss) : mul'' xs yss


-- Right and left justify text
{- If the string is wider than the target length, overflowing characters are put on a new right or left justified line -}
rjustify :: Int -> String -> String
rjustify n xs | length xs <= n   = (replicate k ' ') ++ xs
              | length xs > n    = take n xs ++ "\n" ++ rjustify n (drop n xs)
                 where k = n - (length xs)

ljustify :: Int -> String -> String
ljustify n xs | length xs <= n   = xs ++ (replicate k ' ')
              | length xs > n    = take n xs ++ "\n" ++ ljustify n (drop n xs)
                 where k = n - (length xs)


-- Represent matrix as a right-justified table
output' (n,x) = rjustify n (show x)

table :: Show a => Matrix a -> String
table matrix = unlines (map unwords (listOut matrix))
        where showCols matrix = map (map show) (cols matrix)
              colMax matrix = map maximum (map (map length) (showCols matrix))
              pairs matrix = map (zip (colMax matrix)) matrix
              listOut matrix = map (map output') (pairs matrix)


main = do
   print (cols' [[1,2,3]])
   print (cols'' [[1], [2], [3]])

   print (scale 3 [[1,2,3],[4,5,6]])
   print (scale' 3 [[1,2,3],[4,5,6]])
   print (dot [1,2] [3,4])
   print (dot' [1,2] [3,4])
   print (add [[1,2,3],[4,5,6]] [[7,9,11],[8,10,12]])
   print (mul [[1,2,3],[4,5,6]] [[7,8],[9,10],[11,12]])
   print (mul' [[1,2,3],[4,5,6]] [[7,8],[9,10],[11,12]])
   print (mul'' [[1,2,3],[4,5,6]] [[7,8],[9,10],[11,12]])

   putStr ("\n")
   putStr (rjustify 10 "word")
   putStr ("\n")
   putStr (rjustify 10 "hello world")
   putStr ("\n")
   putStr (ljustify 10 "word")
   putStr ("\n")
   putStr (table  [[1,-500,-4], [100,15043,6], [5,3,10]])

{- Output:
      [[1],[2],[3]]
      [[1,2,3]]
      [[3,6,9],[12,15,18]]
      [[3,6,9],[12,15,18]]
      11
      11
      [[8,11,14],[12,15,18]]
      [58,64,139,154]
      [58,64,139,154]
      [[58,64],[139,154]]
      
            word
      hello worl
               d
      word     
        1  -500 -4 
      100 15043  6
        5     3 10
-}
