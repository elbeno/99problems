module Main where

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Utils
import Q1to10

testMyLast :: (Show a, Eq a) => ([a], a) -> Assertion
testMyLast (xs, expected) =
  expected @=? myLast xs

testMyButLast :: (Show a, Eq a) => ([a], a) -> Assertion
testMyButLast (xs, expected) =
  expected @=? myButLast xs

testElementAt :: (Show a, Eq a) => ([a], Int, a) -> Assertion
testElementAt (xs, n, expected) =
  expected @=? elementAt xs n

testMyLength :: ([a], Int) -> Assertion
testMyLength (xs, expected) =
  expected @=? myLength xs

testMyReverse :: (Show a, Eq a) => ([a], [a]) -> Assertion
testMyReverse (xs, expected) =
  expected @=? myReverse xs

propMyReverse :: [Int] -> Bool
propMyReverse xs = myReverse (myReverse xs) == xs

testIsPalindrome :: (Show a, Eq a) => ([a], Bool) -> Assertion
testIsPalindrome (xs, expected) =
  expected @=? isPalindrome xs

testFlatten :: (Show a, Eq a) => (NestedList a, [a]) -> Assertion
testFlatten (xs, expected) =
  expected @=? flatten xs

testCompress :: (Show a, Eq a) => ([a], [a]) -> Assertion
testCompress (xs, expected) =
  expected @=? compress xs

testPack :: (Show a, Eq a) => ([a], [[a]]) -> Assertion
testPack (xs, expected) =
  expected @=? pack xs

testEncode :: (Show a, Eq a) => ([a], [(Int, a)]) -> Assertion
testEncode (xs, expected) =
  expected @=? encode xs

tests :: [TF.Test]
tests =
  [
    TF.testGroup "Q1to10"
    [
      testWithProvider "Q1: last element 1" testMyLast [([1,2,3,4] :: [Int], 4)],
      testWithProvider "Q1: last element 2" testMyLast [("xyz", 'z')],

      testWithProvider "Q2: last but one element 1" testMyButLast [([1,2,3,4] :: [Int], 3)],
      testWithProvider "Q2: last but one element 2" testMyButLast [(['a'..'z'], 'y')],

      testWithProvider "Q3: kth element 1" testElementAt [([1,2,3] :: [Int], 2, 2)],
      testWithProvider "Q3: kth element 2" testElementAt [("haskell", 5, 'e')],

      testWithProvider "Q4: length 1" testMyLength [([123, 456, 789] :: [Int], 3)],
      testWithProvider "Q4: length 2" testMyLength [("Hello, world!", 13)],

      testWithProvider "Q5: reverse 1" testMyReverse
      [("A man, a plan, a canal, panama!", "!amanap ,lanac a ,nalp a ,nam A")],
      testWithProvider "Q5: reverse 2" testMyReverse [([1,2,3,4] :: [Int], [4,3,2,1])],

      testProperty "Q5: reverse" propMyReverse,

      testWithProvider "Q6: isPalindrome 1" testIsPalindrome [([1,2,3] :: [Int], False)],
      testWithProvider "Q6: isPalindrome 2" testIsPalindrome [("madamimadam", True)],
      testWithProvider "Q6: isPalindrome 3" testIsPalindrome
      [([1,2,4,8,16,8,4,2,1] :: [Int], True)],

      testWithProvider "Q7: flatten 1" testFlatten [(Elem (5 :: Int), [5])],
      testWithProvider "Q7: flatten 2" testFlatten
      [(List [Elem (1 ::Int), List [Elem 2, List [Elem 3, Elem 4], Elem 5]], [1,2,3,4,5])],
      testWithProvider "Q7: flatten 3" testFlatten [(List [] :: NestedList Int, [])],

      testWithProvider "Q8: compress" testCompress [("aaaabccaadeeee", "abcade")],

      testWithProvider "Q9: pack" testPack
      [("aaaabccaadeeee", ["aaaa","b","cc","aa","d","eeee"])],

      testWithProvider "Q10: encode" testEncode
      [("aaaabccaadeeee", [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])]
    ]
  ]

main :: IO ()
main = TF.defaultMain tests
