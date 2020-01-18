module Quiver.Row.Product.Intro where

import GHC.Types
import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Entail
import Quiver.Implicit.Param
import Quiver.Row.Product.Product

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
     . (ImplicitParam k label (Identity (f e)))
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
      => (a ⊗ b) f
    introProduct = introProduct ⊗ introProduct

type Constructor row a =
  ( ProductRow row
  , IntroProduct a
  , Entails
      (ProductConstraint row Identity Identity)
      (ProductConstraint a Identity Identity)
  )

constructProduct
  :: forall row a
   . ( Constructor row a
     , ProductConstraint row Identity Identity
     )
  => a Identity
constructProduct =
  withEntail
    @(ProductConstraint row Identity Identity)
    @(ProductConstraint a Identity Identity) $
      introProduct @a

castConstructor
  :: forall row1 row2 a r
   . ( ProductRow row1
     , ProductRow row2
     , ProductRow a
     , Entails
        (ProductConstraint row1 Identity Identity)
        (ProductConstraint row2 Identity Identity)
     , Entails
        (ProductConstraint row2 Identity Identity)
        (ProductConstraint a Identity Identity)
     )
  => ((Constructor row1 a) => r)
  -> ((Constructor row2 a) => r)
castConstructor cont =
  joinEntail
    @(ProductConstraint row1 Identity Identity)
    @(ProductConstraint row2 Identity Identity)
    @(ProductConstraint a Identity Identity) $
      cont

constructField
  :: forall k label e r
   . e
  -> ((RowConstraint (Field k label e Identity) Identity) => r)
  -> r
constructField x cont =
  withParam @k @label (Identity (Identity x)) cont

constructNamedField
  :: forall label e r
   . e
  -> ((RowConstraint (NamedField label e Identity) Identity) => r)
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
   . ( ProductRow row1
     , ProductRow row2
     , IntroProduct a
     , Entails
         (ProductConstraint
            (Field k label e ⊗ row1)
            Identity
            Identity)
         (ProductConstraint row2 Identity Identity)
     )
  => e
  -> ((Constructor row1 a) => r)
  -> ((Constructor row2 a) => r)
weakenConstruct x cont =
  constructField @k @label x $
    joinEntail
      @(ProductConstraint row1 Identity Identity)
      @(ProductConstraint (Field k label e ⊗ row1) Identity Identity)
      @(ProductConstraint row2 Identity Identity) $
      castConstructor
        @row1
        @row2
        @a
        cont