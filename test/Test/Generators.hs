module Test.Generators where

import Numeric.Natural

import Test.QuickCheck

-- Generators for various structures of natural numbers

naturals :: Gen Natural
naturals = arbitrarySizedNatural

positives :: Gen Natural
positives = naturals `suchThat` (/= 0)

naturalPairs :: Gen (Natural, Natural)
naturalPairs = do
            x <- naturals
            y <- naturals
            return (x,y)

positivePairs :: Gen (Natural, Natural)
positivePairs = do
            x <- positives
            y <- positives
            return (x,y)

naturalLists :: Gen [Natural]
naturalLists = listOf naturals

positiveLists :: Gen [Natural]
positiveLists = listOf positives