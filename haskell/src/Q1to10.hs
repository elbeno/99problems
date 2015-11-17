module Q1to10 where

import Data.List

-- Q1: the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "Empty list passed to myLast"

-- Q2: the last but one element of a list
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "List of length <=1 passed to myButLast"

-- Q3: the kth element of a list
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
elementAt [] _ = error "List passed to elementAt is too short"

-- Q4: the length of a list
myLength :: [a] -> Int
myLength = foldl' (const . (+1)) 0

-- Q5: reverse a list
myReverse :: [a] -> [a]
myReverse = myReverse' []
  where myReverse' = foldl' (flip (:))

-- Q6: is a list a palindrome?
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == myReverse l

-- Q7: flatten a nested list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List as) = concatMap flatten as

-- Q8: eliminate consecutive duplicates
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys)
  | x == y = compress (y:ys)
  | otherwise = x : compress (y:ys)

-- Q9: pack consecutive duplicates into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- Q10: run length encoding
encode :: Eq a => [a] -> [(Int, a)]
encode = map addLen . pack
  where addLen l = (length l, head l)
