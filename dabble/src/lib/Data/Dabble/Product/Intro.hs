module Data.Dabble.Product.Intro where

import GHC.Types
import Data.Functor.Identity
import Data.QuasiParam.Label

import Data.Dabble.Row
import Data.Dabble.Field
import Data.Dabble.Entail
import Data.Dabble.Product.Product

type ProductSubRow row1 row2 =
  SubRow'
    (ProductToRow row1)
    (ProductToRow row2)
    Identity
    Identity

class
  (ProductRow a)
  => IntroProduct a where
    introProduct
      :: forall f
       . (ProductConstraint a f Identity)
      => a f

instance IntroProduct (Field k label e) where
  introProduct
    :: forall f
     . (Param k label (Identity (f e)))
    => Field k label e f
  introProduct =
    Field $ runIdentity $
      captureParam @k @label

instance
  ( IntroProduct a
  , IntroProduct b
  )
  => IntroProduct (a ⊗ b) where
    introProduct
      :: forall f
       . ( ProductConstraint a f Identity
         , ProductConstraint b f Identity
         )
      => a ⊗ b ⋄ f
    introProduct = introProduct ⊗ introProduct

type Constructor row1 row2 =
  ( ProductRow row1
  , IntroProduct row2
  , ProductSubRow row1 row2
  )

constructProduct
  :: forall row1 row2
   . ( Constructor row1 row2
     , ProductConstraint row1 Identity Identity
     )
  => row2 Identity
constructProduct =
  withSubRow'
    @(ProductToRow row1)
    @(ProductToRow row2)
    @Identity
    @Identity $
      introProduct @row2

castConstructor
  :: forall row1 row2 row3 r
   . ( ProductRow row1
     , ProductRow row2
     , ProductRow row3
     , ProductSubRow row1 row2
     )
  => ((Constructor row1 row3) => r)
  -> ((Constructor row2 row3) => r)
castConstructor cont =
  joinSubRow
    @(ProductToRow row1)
    @(ProductToRow row2)
    @(ProductToRow row3)
    @Identity
    @Identity
      cont

constructField
  :: forall k label e r
   . e
  -> ((RowConstraint (Field k label e) Identity Identity) => r)
  -> r
constructField x cont =
  withParam @k @label (Identity (Identity x)) cont

constructNamedField
  :: forall label e r
   . e
  -> ((RowConstraint (NamedField label e) Identity Identity) => r)
  -> r
constructNamedField = constructField @Symbol @label

strengthenConstruct
  :: forall k label e row1 row2 r
   . ( Row row1
     , IntroProduct row2
     )
  => ((Constructor
        (Field k label e ⊗ row1)
        row2)
      => r)
  -> (Constructor row1 row2 => r)
strengthenConstruct cont =
    castConstructor
      @(Field k label e ⊗ row1)
      @row1
      @row2
      cont

weakenConstruct
  :: forall k label row1 row2 e r
   . ( ProductRow row1
     , IntroProduct row2
     )
  => e
  -> ((Constructor row1 row2) => r)
  -> ((Constructor (Field k label e ⊗ row1) row2) => r)
weakenConstruct x cont =
  constructField @k @label x $
    joinSubRow
      @(ProductToRow row1)
      @(ProductToRow (Field k label e ⊗ row1))
      @(ProductToRow row2)
      @Identity
      @Identity
      cont

weakenConstruct2
  :: forall k label row1 row2 row3 e r
   . ( ProductRow row1
     , IntroProduct row3
     , ProductSubRow (Field k label e ⊗ row1) row3
     )
  => e
  -> ((Constructor row1 row2) => r)
  -> ((Constructor row3 row2) => r)
weakenConstruct2 x cont =
  castConstructor
    @(Field k label e ⊗ row1)
    @row3
    @row2 $
      weakenConstruct @k @label @row1 @row2 x cont
