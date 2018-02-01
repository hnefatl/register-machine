import Test.Tasty

import Test.Pure
import Test.List
import Test.Program

main :: IO ()
main = defaultMain globalTests

globalTests :: TestTree
globalTests = testGroup "Global"
    [
        pureTests,
        listTests,
        programTests
    ]