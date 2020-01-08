module Quiver.Test.Sum.Elim where

import GHC.Types

import Test.Tasty
import Test.Tasty.HUnit

import Quiver.Row.Field
import Quiver.Row.Sum
import Quiver.Row.Product

sumElimTests :: TestTree
sumElimTests = testGroup "sum elim tests"
  [ test1
  ]

type FooField = NamedField "Foo" String

type BarField = NamedField "Bar" String

type FooBarField = FooField ⊕ BarField

type BarFooField = BarField ⊕ FooField

type FooBarPair = FooField ⊗ BarField

type BarFooPair = BarField ⊗ FooField

fooField :: FooField
fooField = Field "foo"

barField :: BarField
barField = Field "bar"

fooInFooBar :: FooBarField
fooInFooBar = Inl fooField

fooInBarFoo :: BarFooField
fooInBarFoo = Inr fooField

barInFooBar :: FooBarField
barInFooBar = Inr barField

barInBarFoo :: BarFooField
barInBarFoo = Inl barField

matchFooBar
  :: forall a
   . (MatchField
        a
        FooBarField
        String
     )
  => a
  -> String
matchFooBar x = match @a x $
    (caseOf @Symbol @"Foo" @String $
      \val -> "foo val: " ++ val
    )
  ⊗ (caseOf @Symbol @"Bar" @String $
      \val -> "bar val: " ++ val
    )

test1 :: TestTree
test1 = testCase "closed sum matching" $ do
  assertEqual
    "should be able to match foo"
    (matchFooBar fooField)
    "foo val: foo"

  assertEqual
    "should be able to match foo"
    (matchFooBar fooInFooBar)
    "foo val: foo"

  assertEqual
    "should be able to match foo"
    (matchFooBar fooInBarFoo)
    "foo val: foo"

  assertEqual
    "should be able to match bar"
    (matchFooBar barField)
    "bar val: bar"

  assertEqual
    "should be able to match bar"
    (matchFooBar barInFooBar)
    "bar val: bar"

  assertEqual
    "should be able to match bar"
    (matchFooBar barInBarFoo)
    "bar val: bar"