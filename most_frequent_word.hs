-- Used for isLower, isUpper, toLower
import Data.Char

{- mostFrequent finds the n most frequent words in the text ts
   Strings representing text paragraphs are:
    1. formatted to lowercase with spaces and no punctuation (map format)
    2. turned into a list of strings, which are the words (words')
    3. the list of words is quicksorted (qsort)
    4. adjacent identiacal words are grouped to form a 2D list (group')
    5. the groups in the list are now represented by a (word, no. occurences) tuple (map rep)
    6. the list of tuples is quicksorted by increasing no. occurences of each word (qsortTup)
    7. list is reversed (reverse)
    8. first n entries are taken (take n)
-}
mostFrequent :: Int -> [Char] -> [([Char], Int)]
mostFrequent n ts = take n (reverse (qsortTup (map rep (group' (qsort (words' (map format ts)))))))

-- Turns characters to lower case, and punctation to spaces
format :: Char -> Char
format t | isLower t   = t
         | isUpper t   = toLower t
         | otherwise   = ' '

-- Turns a string into a list of strings that are words
words' :: [Char] -> [[Char]]
words' [] = []
words' xs = [takeWhile (/= ' ') xs] ++ words (dropWhile (/= ' ') xs)

-- Quicksort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort [y | y <- xs, y < x]) ++ (x : [y | y <- xs, y == x]) ++ (qsort [y | y <- xs, y > x])

-- Quicksort (word, frequency) in increasing order based on frequency
qsortTup :: Ord a => [(b, a)] -> [(b, a)]
qsortTup [] = []
qsortTup ((m,n):xs) = (qsortTup [(p, q) | (p, q) <- xs, q < n]) ++ ((m,n) : [(p, q) | (p, q) <- xs, q == n]) ++ (qsortTup [(p, q) | (p, q) <- xs, q > n])


-- Group adjacent elements that are identical
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:xs) = [x : (takeWhile (== x) xs)] ++ (group' (dropWhile (== x) xs))

-- Find the number of elements in a list of identical items
rep :: [a] -> (a, Int)
rep xs = (head xs, length xs)


main = do
  print (mostFrequent 10 "Lo! the Spear-Danes’ glory through splendid achievements\nThe folk-kings’ former fame we have heard of,\nHow princes displayed then their prowess-in-battle.\nOft Scyld the Scefing from scathers in numbers\nFrom many a people their mead-benches tore.\nSince first he found him friendless and wretched,\nThe earl had had terror: comfort he got for it,\nWaxed ’neath the welkin, world-honor gained,\nTill all his neighbors o’er sea were compelled to\nBow to his bidding and bring him their tribute:\nAn excellent atheling! After was borne him\nA son and heir, young in his dwelling,\nWhom God-Father sent to solace the people.\nHe had marked the misery malice had caused them,\nThat reaved of their rulers they wretched had erstwhile\nLong been afflicted. The Lord, in requital,\nWielder of Glory, with world-honor blessed him.\nFamed was Beowulf, far spread the glory\nOf Scyld’s great son in the lands of the Danemen.\nSo the carle that is young, by kindnesses rendered\nThe friends of his father, with fees in abundance\nMust be able to earn that when age approacheth\nEager companions aid him requitingly,\nWhen war assaults him serve him as liegemen:\nBy praise-worthy actions must honor be got\n’Mong all of the races. At the hour that was fated\nScyld then departed to the All-Father’s keeping\nWarlike to wend him; away then they bare him\nTo the flood of the current, his fond-loving comrades,\nAs himself he had bidden, while the friend of the Scyldings\nWord-sway wielded, and the well-lovèd land-prince\nLong did rule them. The ring-stemmèd vessel,\nBark of the atheling, lay there at anchor,\nIcy in glimmer and eager for sailing;\nThe belovèd leader laid they down there,\nGiver of rings, on the breast of the vessel,\nThe famed by the mainmast. A many of jewels,\nOf fretted embossings, from far-lands brought over,\nWas placed near at hand then; and heard I not ever\nThat a folk ever furnished a float more superbly\nWith weapons of warfare, weeds for the battle,\nBills and burnies; on his bosom sparkled\nMany a jewel that with him must travel\nOn the flush of the flood afar on the current.\nAnd favors no fewer they furnished him soothly,\nExcellent folk-gems, than others had given him\nWho when first he was born outward did send him\nLone on the main, the merest of infants:\nAnd a gold-fashioned standard they stretched under heaven\nHigh o’er his head, let the holm-currents bear him,\nSeaward consigned him: sad was their spirit,\nTheir mood very mournful. Men are not able\nSoothly to tell us, they in halls who reside,\nHeroes under heaven, to what haven he hied.")

  {- With thanks to Project Gutenburg: https://www.gutenberg.org/files/16328/16328-h/16328-h.htm (Accessed 19/08/24) -}
