module Quiver.Test.Sum.Intro where

import GHC.Types

import Test.Tasty
import Test.Tasty.HUnit

import Quiver.Row.Field
import Quiver.Row.Sum

sumIntroTests :: TestTree
sumIntroTests = testGroup "sum intro tests"
  [ test1
  ]

type FooField = NamedField "Foo" String

type BarField = NamedField "Bar" String

type FooBarField = FooField ⊕ BarField

type BarFooField = BarField ⊕ FooField

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

makeFoo
  :: forall a
   . (ConstructSum FooField a String)
  => a
makeFoo =
  constructSum @Symbol @"Foo" @a @String "foo"

makeBar
  :: forall a
   . (ConstructSum BarField a String)
  => a
makeBar =
  constructSum @Symbol @"Bar" @a @String "bar"

test1 :: TestTree
test1 = testCase "constructors" $ do
  assertEqual
    "should be able to make foo field"
    makeFoo
    fooField

  assertEqual
    "should be able to make foo field"
    makeFoo
    fooInFooBar

  assertEqual
    "should be able to make foo field"
    makeFoo
    fooInBarFoo

  assertEqual
    "should be able to make bar field"
    makeBar
    barField

  assertEqual
    "should be able to make bar field"
    makeBar
    barInFooBar

  assertEqual
    "should be able to make bar field"
    makeBar
    barInBarFoo
