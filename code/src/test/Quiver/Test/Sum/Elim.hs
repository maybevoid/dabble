module Quiver.Test.Sum.Elim where

import Data.Functor.Identity
import GHC.Types

import Test.Tasty
import Test.Tasty.HUnit

import Quiver.Row.Field
import Quiver.Row.Row
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
  => a Identity
makeFoo =
  constructSum @Symbol @"Foo" @a @String "foo"

makeBar
  :: forall a
   . (ConstructSum BarField a String)
  => a Identity
makeBar =
  constructSum @Symbol @"Bar" @a @String "bar"

makeBaz
  :: forall a
   . (ConstructSum BazField a String)
  => a Identity
makeBaz =
  constructSum @Symbol @"Baz" @a @String "baz"

fooField :: FooField Identity
fooField = makeFoo

fooInFooBar :: FooBarField Identity
fooInFooBar = makeFoo

fooInBarFoo :: BarFooField Identity
fooInBarFoo = makeFoo

fooInFooBarBaz :: FooBarBazField Identity
fooInFooBarBaz = makeFoo

fooInBazBarFoo :: BazBarFooField Identity
fooInBazBarFoo = makeFoo

barField :: BarField Identity
barField = makeBar

barInFooBar :: FooBarField Identity
barInFooBar = makeBar

barInBarFoo :: BarFooField Identity
barInBarFoo = makeBar

bazInFooBarBaz :: FooBarBazField Identity
bazInFooBarBaz = makeBaz

bazInBazBarFoo :: BazBarFooField Identity
bazInBazBarFoo = makeBaz

matchFooBar
  :: forall a
   . (Match a FooBarField)
  => a Identity
  -> String
matchFooBar x = match @a @FooBarField x $
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
  => a Identity
  -> String
openMatchFooBar x =
  openMatch @a1 @a2 @a @FooBarField x
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
        @Bottom
        fooField
    )
    "foo val: foo"

  assertEqual
    "should be able to match foo"
    ( openMatchFooBar
        @FooBarField
        @Bottom
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