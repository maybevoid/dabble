import Test.Tasty

import Quiver.Test.Product.Elim
import Quiver.Test.Product.Intro
import Quiver.Test.Sum.Intro
import Quiver.Test.Sum.Elim

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Quiver Rows Tests"
  [ productElimTests
  , productIntroTests
  , sumIntroTests
  , sumElimTests
  ]
