module Quiver.Test.Product.Intro where

import GHC.Types

import Test.Tasty
import Test.Tasty.HUnit

import Quiver.Row.Field
import Quiver.Row.Product

productIntroTests :: TestTree
productIntroTests = testGroup "product intro tests"
  [ test1
  , test2
  , test3
  ]

type FooField = NamedField "Foo" String

type BarField = NamedField "Bar" String

type FooBarField = FooField ⊗ BarField
type BarFooField = BarField ⊗ FooField

fooField :: FooField
fooField = Field "foo"

barField :: BarField
barField = Field "bar"

fooBarPair :: FooField ⊗ BarField
fooBarPair = fooField ⊗ barField

barFooPair :: BarField ⊗ FooField
barFooPair = barField ⊗ fooField

newtype Foo = Foo String

newtype Bar = Bar String

data FooBar = FooBar
  { foo :: String
  , bar :: String
  }
  deriving (Eq)

makeFoo
  :: forall a
   . (Constructor FooField a)
  => a
makeFoo =
  constructNamedField @"Foo" "foo" $
    constructProduct @FooField @a

makeFooBar
  :: forall a
   . (Constructor FooBarField a)
  => a
makeFooBar =
  constructNamedField @"Foo" "foo" $
    constructNamedField @"Bar" "bar" $
      constructProduct @FooBarField @a

makeFoo2
  :: forall a
   . (Constructor FooField a)
  => a
makeFoo2 =
  castConstructor
    @FooBarField
    @FooField
    @a
    makeFooBar

makeFooBar2
  :: forall a
   . (Constructor FooBarField a)
  => a
makeFooBar2 =
  weakenConstruct
    @Symbol
    @"Bar"
    @FooField
    @FooBarField
    @a
    "bar"
    makeFoo

test1 :: TestTree
test1 = testCase "construct foo" $ do
  assertEqual
    "should be able to make foo field"
    makeFoo
    fooField

  assertEqual
    "should be able to make foo field"
    makeFoo2
    fooField

test2 :: TestTree
test2 = testCase "construct foo bar" $ do
  assertEqual
    "should be able to make foo bar field"
    makeFooBar
    fooBarPair

  assertEqual
    "should be able to make foo bar field"
    makeFooBar
    barFooPair

  assertEqual
    "should be able to make foo bar field"
    makeFooBar2
    fooBarPair

  assertEqual
    "should be able to make foo bar field"
    makeFooBar2
    barFooPair


test3 :: TestTree
test3 = testCase "cast test" $ do
  assertEqual
    "should be able to cast foo bar to foo"
    (castProduct fooBarPair)
    fooField

  assertEqual
    "should be able to cast foo bar to bar"
    (castProduct fooBarPair)
    fooField

  assertEqual
    "should be able to cast bar foo to foo"
    (castProduct barFooPair)
    fooField

  assertEqual
    "should be able to cast bar foo to bar"
    (castProduct barFooPair)
    fooField

  assertEqual
    "should be able to cast foo bar to bar foo"
    (castProduct fooBarPair)
    barFooPair

  assertEqual
    "should be able to cast bar foo to foo bar"
    (castProduct barFooPair)
    fooBarPair

  assertEqual
    "should be able to cast foo bar to same foo bar"
    (castProduct fooBarPair)
    fooBarPair
