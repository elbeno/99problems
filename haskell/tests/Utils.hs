module Utils where

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH
import Test.HUnit

testWithProvider :: String -> (a -> Assertion) -> [a] -> TF.Test
testWithProvider testGroupName testFunction =
    TF.testGroup testGroupName . map createTest . zipWith assignName [1::Int ..]
      where
        createTest (name, dataSet)   = TFH.testCase name $ testFunction dataSet
        assignName setNumber dataSet = ("Data set " ++ show setNumber, dataSet)
