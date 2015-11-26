module Q11to20 where

import Q1to10

-- Q11: modified run-length encoding
data RLE a = Multiple Int a
           | Single a
           deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [RLE a]
encodeModified = map listToRLE . pack
  where listToRLE [x] = Single x
        listToRLE xs = Multiple (length xs) (head xs)

-- Q12: decode a run-length encoded list
rleToList :: RLE a -> [a]
rleToList (Single x) = [x]
rleToList (Multiple n x) = replicate n x

decodeModified :: [RLE a] -> [a]
decodeModified = concatMap rleToList

-- Q13: run-length encoding (direct solution)
toRLE :: Int -> a -> RLE a
toRLE 1 x = Single x
toRLE n x = Multiple n x

encodeDirect :: Eq a => [a] -> [RLE a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = toRLE n x : encodeDirect t
  where n = 1 + length (takeWhile (==x) xs)
        t = dropWhile (==x) xs

-- Q14: duplicate elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Q15: replicate the elements of a list
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Q16: drop every nth element
dropEvery :: [a] -> Int -> [a]
dropEvery l n = dropEvery' l n
  where dropEvery' [] _ = []
        dropEvery' (_:xs) 1 = dropEvery' xs n
        dropEvery' (x:xs) k = x : dropEvery' xs (k-1)

-- Q17: split a list into two parts
split :: [a] -> Int -> ([a], [a])
split = split' []
  where split' f [] _ = (f, [])
        split' f b 0 = (f, b)
        split' f (x:xs) n = split' (f++[x]) xs (n-1)

-- Q18: extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice l a b = take (b-a+1) $ drop (a-1) l

-- Q19: rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate l n = b ++ f
  where n' = n `mod` length l
        f = take n' l
        b = drop n' l

-- Q20: remove the kth element from a list
removeAt :: [a] -> Int -> (a, [a])
removeAt l n = (head b, f ++ tail b)
  where (f, b) = split l (n-1)
