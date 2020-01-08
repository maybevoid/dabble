module Quiver.Row.Product.Intro where

import GHC.Types
import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Entail
import Quiver.Implicit.Param
import Quiver.Row.Product.Product

class
  (Row a)
  => IntroProduct a where
    introProduct
      :: (RowConstraint a Identity)
      => a

instance IntroProduct (Field k label e) where
  introProduct
    :: ((ImplicitParam k label (Identity e))
        => Field k label e)
  introProduct =
    Field $ runIdentity $
      captureParam @k @label

instance
  ( IntroProduct a
  , IntroProduct b
  )
  => IntroProduct (a ⊗ b) where
    introProduct
      :: ( RowConstraint a Identity
         , RowConstraint b Identity
         )
      => a ⊗ b
    introProduct = introProduct ⊗ introProduct

type Constructor row a =
  ( Row row
  , IntroProduct a
  , Entails
      (RowConstraint row Identity)
      (RowConstraint a Identity)
  )

constructProduct
  :: forall row a
   . ( Constructor row a
     , RowConstraint row Identity
     )
  => a
constructProduct =
  withEntail
    @(RowConstraint row Identity)
    @(RowConstraint a Identity) $
      introProduct @a

castConstructor
  :: forall row1 row2 a r
   . ( Row row1
     , Row row2
     , Entails
        (RowConstraint row1 Identity)
        (RowConstraint row2 Identity)
     , Entails
        (RowConstraint row2 Identity)
        (RowConstraint a Identity)
     )
  => ((Constructor row1 a) => r)
  -> ((Constructor row2 a) => r)
castConstructor cont =
  joinEntail
    @(RowConstraint row1 Identity)
    @(RowConstraint row2 Identity)
    @(RowConstraint a Identity) $
      cont

constructField
  :: forall k label e r
   . e
  -> ((RowConstraint (Field k label e) Identity) => r)
  -> r
constructField x cont =
  withParam @k @label (Identity x) cont

constructNamedField
  :: forall label e r
   . e
  -> ((RowConstraint (NamedField label e) Identity) => r)
  -> r
constructNamedField = constructField @Symbol @label

strengthenConstruct
  :: forall k label e row a r
   . ( Row row
     , IntroProduct a
     )
  => e
  -> ((Constructor
        (Field k label e ⊗ row)
        a)
      => r)
  -> (Constructor row a => r)
strengthenConstruct x cont =
  withParam @k @label (Identity x) $
    castConstructor
      @(Field k label e ⊗ row)
      @row
      @a
      cont

weakenConstruct
  :: forall k label row1 row2 a e r
   . ( Row row1
     , Row row2
     , IntroProduct a
     , Entails
         (RowConstraint
            (Field k label e ⊗ row1)
            Identity)
         (RowConstraint row2 Identity)
     )
  => e
  -> ((Constructor row1 a) => r)
  -> ((Constructor row2 a) => r)
weakenConstruct x cont =
  constructField @k @label x $
    joinEntail
      @(RowConstraint row1 Identity)
      @(RowConstraint (Field k label e ⊗ row1) Identity)
      @(RowConstraint row2 Identity) $
      castConstructor
        @row1
        @row2
        @a
        cont