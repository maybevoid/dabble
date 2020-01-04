module Quiver.Test.Product.Intro where

import Test.Tasty
import Test.Tasty.HUnit

import Quiver.Row.Row
import Quiver.Row.Product.Intro
import Quiver.Row.Product.Product

productIntroTests :: TestTree
productIntroTests = testGroup "product intro tests"
  [
  ]

type FooField = NamedField "Foo" String

type BarField = NamedField "Bar" String

type FooBarField = Product FooField BarField

fooField :: FooField
fooField = Field "foo"

barField :: BarField
barField = Field "bar"

fooBarPair
  :: Product
      (NamedField "Foo" String)
      (NamedField "Bar" String)
fooBarPair = Product fooField barField

barFooPair
  :: Product
      (NamedField "Bar" String)
      (NamedField "Foo" String)
barFooPair = Product barField fooField

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

makeFoo3
  :: forall a
   . (Constructor FooBarField a)
  => a
makeFoo3 =
  constructNamedField @"Bar" "bar" $
    castConstructor
      @FooField
      @FooBarField
      @a
      makeFoo