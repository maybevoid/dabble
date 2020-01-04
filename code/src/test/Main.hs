import Test.Tasty

import Quiver.Test.Product.Elim
import Quiver.Test.Product.Intro

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Quiver Rows Tests"
  [ productElimTests
  , productIntroTests
  ]
