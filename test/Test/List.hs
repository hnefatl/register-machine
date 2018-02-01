module Test.List where

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Generators

import RM.List

listTests :: TestTree
listTests = testGroup "List Tests"
    [
        testProperty "decodeList . encodeList == id" $ forAll naturalLists (\p -> (decodeList . encodeList) p == p),
        testProperty "encodeList . decodeList == id" $ forAll naturals (\p -> (encodeList . decodeList) p == p)
    ]