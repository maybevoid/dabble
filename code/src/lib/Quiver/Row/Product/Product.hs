{-# LANGUAGE UndecidableInstances #-}

module Quiver.Row.Product.Product where

import Data.Kind
import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Entail
import Quiver.Row.Field
import Quiver.Implicit.Param

data Product a b (f :: Type -> Type)
  = Product (a f) (b f)
  deriving (Eq, Show)

type ProductConstraint row f t =
  RowConstraint (ProductToRow row) f t

type SubProduct row1 row2 =
  SubRow (ProductToRow row1) (ProductToRow row2)

class
  (Row (ProductToRow row))
  => ProductRow
    (row :: (Type -> Type) -> Type)
  where
    type family ProductToRow row
      :: (Type -> Type) -> Type

    withProduct
      :: forall f r
      . row f
      -> (ProductConstraint row f Identity => r)
      -> r

infixr 7 ⊗

type a ⊗ b = Product a b

(⊗) :: a f -> b f -> (a ⊗ b) f
(⊗) = Product

instance ProductRow Top where
  type ProductToRow Top = Top

  withProduct Top cont = cont

instance ProductRow (Field k label e) where
  type ProductToRow (Field k label e) =
    Field k label e

  withProduct
    :: forall f r
     . Field k label e f
    -> (ImplicitParam k label (Identity (f e)) => r)
    -> r
  withProduct (Field e) cont =
    withParam @k @label (Identity e) cont

instance
  ( ProductRow a
  , ProductRow b
  ) => ProductRow (a ⊗ b) where
    type ProductToRow (a ⊗ b) =
      (ProductToRow a) ∪ (ProductToRow b)

    withProduct
      :: forall f r
       . (a ⊗ b) f
      -> (( ProductConstraint a f Identity
          , ProductConstraint b f Identity
          ) => r)
      -> r
    withProduct (Product a b) cont =
      withProduct a $
        withProduct b $
          cont

first :: forall f a b . (a ⊗ b) f -> a f
first (Product a _) = a

second :: forall f a b . (a ⊗ b) f -> b f
second (Product _ b) = b
