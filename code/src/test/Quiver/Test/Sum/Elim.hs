module Quiver.Test.Sum.Elim where

import Data.Void
import GHC.Types

import Test.Tasty
import Test.Tasty.HUnit

import Quiver.Row.Field
import Quiver.Row.Sum
import Quiver.Row.Product

sumElimTests :: TestTree
sumElimTests = testGroup "sum elim tests"
  [ test1
  , test2
  , test3
  ]

type FooField = NamedField "Foo" String

type BarField = NamedField "Bar" String

type BazField = NamedField "Baz" String

type FooBarField = FooField ⊕ BarField

type BarFooField = BarField ⊕ FooField

type FooBarBazField = FooField ⊕ BarField ⊕ BazField

type BazBarFooField = BazField ⊕ BarField ⊕ FooField

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

makeBaz
  :: forall a
   . (ConstructSum BazField a String)
  => a
makeBaz =
  constructSum @Symbol @"Baz" @a @String "baz"

fooField :: FooField
fooField = makeFoo

fooInFooBar :: FooBarField
fooInFooBar = makeFoo

fooInBarFoo :: BarFooField
fooInBarFoo = makeFoo

fooInFooBarBaz :: FooBarBazField
fooInFooBarBaz = makeFoo

fooInBazBarFoo :: BazBarFooField
fooInBazBarFoo = makeFoo

barField :: BarField
barField = makeBar

barInFooBar :: FooBarField
barInFooBar = makeBar

barInBarFoo :: BarFooField
barInBarFoo = makeBar

bazInFooBarBaz :: FooBarBazField
bazInFooBarBaz = makeBaz

bazInBazBarFoo :: BazBarFooField
bazInBazBarFoo = makeBaz

matchFooBar
  :: forall a
   . (Match a FooBarField)
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

openMatchFooBar
  :: forall a1 a2 a
   . (OpenMatch a1 a2 a FooBarField)
  => a
  -> String
openMatchFooBar x =
  openMatch @a1 @a2 x
    ( (caseOf @Symbol @"Foo" @String $
        \val -> "foo val: " ++ val
      )
    ⊗ (caseOf @Symbol @"Bar" @String $
        \val -> "bar val: " ++ val
      )
    )
    (\_ -> "unknown value")

test2 :: TestTree
test2 = testCase "open sum partition" $ do
  assertEqual
    "should be partition to left"
    (partitionSum @FooBarField @FooField @BarField fooInFooBar)
    (Left fooField)

  assertEqual
    "should be partition to left"
    (partitionSum @FooBarField @BarField @FooField fooInFooBar)
    (Right fooField)

test3 :: TestTree
test3 = testCase "open sum matching" $ do
  assertEqual
    "should be able to match foo"
    ( openMatchFooBar
        @FooField
        @Void
        fooField
    )
    "foo val: foo"

  assertEqual
    "should be able to match foo"
    ( openMatchFooBar
        @FooBarField
        @Void
        fooInFooBar
    )
    "foo val: foo"

  assertEqual
    "should be able to match foo"
    ( openMatchFooBar
        @FooBarField
        @BazField
        fooInFooBarBaz
    )
    "foo val: foo"

  assertEqual
    "should be able to match foo"
    ( openMatchFooBar
        @BarFooField
        @BazField
        fooInBazBarFoo
    )
    "foo val: foo"

  assertEqual
    "should be able to match baz"
    ( openMatchFooBar
        @FooBarField
        @BazField
        bazInFooBarBaz
    )
    "unknown value"

  assertEqual
    "should be able to match baz"
    ( openMatchFooBar
        @BarFooField
        @BazField
        bazInBazBarFoo
    )
    "unknown value"