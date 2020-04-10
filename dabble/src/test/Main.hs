import Test.Tasty

import Dabble.Test.Product.Elim
import Dabble.Test.Product.Intro
import Dabble.Test.Sum.Intro
import Dabble.Test.Sum.Elim

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Dabble Rows Tests"
  [ productElimTests
  , productIntroTests
  , sumIntroTests
  , sumElimTests
  ]
