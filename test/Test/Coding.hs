module Test.Coding where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Natural

import Coding

codingTests :: TestTree
codingTests = testGroup "Coding Tests"
    [
        testProperty "encode1 . decode1 == id" $ forAll positives (\n -> (encode1 . decode1) n == n),
        testProperty "decode1 . encode1 == id" $ forAll positivePairs (\p -> (decode1 . encode1) p == p),

        testProperty "encode0 . decode0 == id" $ forAll naturals (\n -> (encode0 . decode0) n == n),
        testProperty "decode0 . encode0 == id" $ forAll naturalPairs (\p -> (decode0 . encode0) p == p)
    ]