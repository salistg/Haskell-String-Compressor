----------------------------------------------------------------
-- Haskell Coursework 2
----------------------------------------------------------------
-- PART 1:
-- Compression

-- Exercise 1: Chomp function
--             selects a run of repeated characters from the start of a string
--             with the run being as long as possible
chomp :: String -> String
chomp inputString = takeWhile (== head inputString) inputString

-- Exercise 2: munch function using chomp
--             selects a run of repeated characters from the start of a string
--             with the run comprising nine characters at most
munch :: String -> String
munch = take 9 . chomp 

-- Exercise 3: runs function using munch
--             splits a string into a list of runs of repeated characaters
--             with each run comprising nine characters a most
runs :: String -> [String]
runs "" = []
runs xs = x : runs (drop (length x) xs)
      where
          x = munch xs

-- Exercise 4: encode function using runs
--             transforms a string into a list of pairs
--             compromising the character from each run
--             together with its number of repitions
encode :: String -> [(Char,Int)]
encode [] = []
encode xs = [(head x, length x) | x <- runs xs]

-- Exercise 5: flatten function
--             flattens a list of pairs of characters and digits to a string
flatten :: [(Char,Int)] -> String
flatten [] = []
flatten (x:xs) = (fst x): (show (snd x) ++ flatten xs)

-- Exercise 6: compress function using encode and flaten
--             compresses a string using run-length encoding
compress :: String -> String
compress [] = []
compress inputString = flatten (encode inputString)

-- PART 2:
-- Decompression

-- Exercise 7: decode function
--             performs the inverse of the encode function
decode :: [(Char, Int)] -> String
decode [] = []
decode (x:xs) = ((take (snd x) (repeat (fst x)))) ++ decode xs

-- Exercise 8: expand function
--             performs the inverse of the flatten function
expand :: String -> [(Char, Int)]
expand [] = []
expand [x] = []
expand (x:y:xs) = (x,read [y]):(expand xs)

-- Exercise 9: decompress function using decode and expand
--             performes the inverse of the compress function
decompress :: String -> String
decompress [] = []
decompress inputString = decode (expand inputString)




