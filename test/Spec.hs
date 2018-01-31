import Test.Tasty

import Test.Coding

main :: IO ()
main = defaultMain globalTests

globalTests :: TestTree
globalTests = testGroup "Global"
    [
        codingTests
    ]