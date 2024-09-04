{- Type synonyms:
    - Grid shows each position and its entry
    - A Path is a sequence of Grids
-}
type Coordinates = (Int, Int)
type Grid = [(Coordinates, Int)]
type Path = [Grid]


{- Some grids, including examples to be solved and the solution grid
      1 2 3           1 0 2           0 8 1           4 1 3           1 2 3
      0 4 6           4 5 3           2 3 4           0 2 5           4 5 6
      7 5 8           7 8 6           6 7 5           7 8 6           7 8 0
  Puzzle not possible if number of inversions odd, such as in example3 which never terminates
-}
example1 =
 [((0,0),7), ((1,0),5), ((2,0),8), ((0,1),0), ((1,1),4), ((2,1),6), ((0,2),1), ((1,2),2), ((2,2),3)]

example2 =
 [((0,0),7), ((1,0),8), ((2,0),6), ((0,1),4), ((1,1),5), ((2,1),3), ((0,2),1), ((1,2),0), ((2,2),2)]

example3 =
 [((0,0),6), ((1,0),7), ((2,0),5), ((0,1),2), ((1,1),3), ((2,1),4), ((0,2),0), ((1,2),8), ((2,2),1)]

example4 =
 [((0,0),7), ((1,0),8), ((2,0),6), ((0,1),0), ((1,1),2), ((2,1),5), ((0,2),4), ((1,2),1), ((2,2),3)]

solution =
 [((0,0),7), ((1,0),8), ((2,0),0), ((0,1),4), ((1,1),5), ((2,1),6), ((0,2),1), ((1,2),2), ((2,2),3)]


{- Turn Grids and Paths into strings to be displayed in the solution
-}
showGrid :: Grid -> String
showGrid grid = unwords line3 ++ "\n" ++ unwords line2 ++ "\n" ++ unwords line1 ++ "\n\n"
        where numbs = map (\(cs,n) -> n) grid
              line1 = map show (take 3 numbs)
              line2 = map show (take 3 (drop 3 numbs))
              line3 = map show (drop 6 numbs)

showPath :: Path -> String
showPath [] = ""
showPath (xs:xss) = showGrid xs ++ showPath xss

showSolution :: Grid -> IO()
showSolution grid = putStr("====== Solution ======\nTotal Depth Searched: " ++ show (length solution - 1) ++ "\n\n" ++ showPath solution)
      where solution = solve grid


{- Solves a given eight_puzzle using a bfs
   Initially, only the starting grid has been seen, which forms the only path so far
-}
solve :: Grid -> Path
solve grid = reverse (solver [[grid]] [grid])


{- seen represents the grids that have already been visited to avoid re-visiting them which can take lots of time
-}
solver :: [Path] -> [Grid] -> Path
solver [] seen = []
solver (xs:xss) seen | currentGrid == solution = xs
                     | otherwise               = solver (xss ++ newPaths) (seen ++ [ns] ++ [ss] ++ [es] ++ [ws])
        where currentGrid = head xs
              zeroEntry = head (dropWhile (\((_, _),z) -> z /= 0) currentGrid)
              (zeroX, zeroY) = (\((x,y),_) -> (x,y)) zeroEntry
              tempns = if zeroY == 2 then [] else change (zeroX, zeroY) (zeroX, zeroY+1) currentGrid
              tempss = if zeroY == 0 then [] else change (zeroX, zeroY) (zeroX, zeroY-1) currentGrid
              tempes = if zeroX == 2 then [] else change (zeroX, zeroY) (zeroX+1, zeroY) currentGrid
              tempws = if zeroX == 0 then [] else change (zeroX, zeroY) (zeroX-1, zeroY) currentGrid
              ns = if tempns `elem` seen then [] else tempns
              ss = if tempss `elem` seen then [] else tempss
              es = if tempes `elem` seen then [] else tempes
              ws = if tempws `elem` seen then [] else tempws
              newPaths = filter (/=([]:xs)) [ns:xs, ss:xs, es:xs, ws:xs]


{- Swaps the entries in the positions of the old and new coordinates:

    _______________________grid_______________________
    
               fstChange           sndChange
     __start__   old/new   __mid__   old/new   __end__
                 __________________r1_________________
                                     ________r2_______
-}
change :: Coordinates -> Coordinates -> Grid -> Grid
change old new grid = start ++ [(fstToChange, snd)] ++ mid ++ [(sndToChange, fst)] ++ end
      where (start, r1) = break (\(cs,_) -> cs == old || cs == new) grid
            (fstToChange, fst) = head r1
            (mid, r2) = break (\(cs,_) -> cs == old || cs == new) (tail r1)
            (sndToChange, snd) = head r2
            end = tail r2


main = do
  showSolution(example1)
  showSolution(example2)
  --showSolution(example3)    Doesn't terminate in a reasonable amount of time
  showSolution(example4)
  showSolution(solution)




{- Expected output:
====== Solution ======
Total Depth Searched: 3

1 2 3
0 4 6
7 5 8

1 2 3
4 0 6
7 5 8

1 2 3
4 5 6
7 0 8

1 2 3
4 5 6
7 8 0

====== Solution ======
Total Depth Searched: 3

1 0 2
4 5 3
7 8 6

1 2 0
4 5 3
7 8 6

1 2 3
4 5 0
7 8 6

1 2 3
4 5 6
7 8 0

====== Solution ======
Total Depth Searched: 5

4 1 3
0 2 5
7 8 6

0 1 3
4 2 5
7 8 6

1 0 3
4 2 5
7 8 6

1 2 3
4 0 5
7 8 6

1 2 3
4 5 0
7 8 6

1 2 3
4 5 6
7 8 0

====== Solution ======
Total Depth Searched: 0

1 2 3
4 5 6
7 8 0

-}
