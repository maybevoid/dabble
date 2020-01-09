module Quiver.Row.Product.Product where

import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Implicit.Param

data Product a b = Product a b
  deriving (Eq, Show)

type ProductConstraint row f =
  RowConstraint (ProductToRow row) f

class
  (Row (ProductToRow row))
  => ProductRow row where
    type family ProductToRow row

    withProduct
      :: forall r
      . row
      -> (ProductConstraint row Identity => r)
      -> r

infixr 7 ⊗

type a ⊗ b = Product a b

(⊗) :: a -> b -> a ⊗ b
(⊗) = Product

instance ProductRow () where
  type ProductToRow () = ()

  withProduct () cont = cont

instance ProductRow (Field k label e) where
  type ProductToRow (Field k label e) =
    Field k label e

  withProduct
    :: forall r
     . Field k label e
    -> (ImplicitParam k label (Identity e) => r)
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
      :: forall r
       . a ⊗ b
      -> (( ProductConstraint a Identity
          , ProductConstraint b Identity
          ) => r)
      -> r
    withProduct (Product a b) cont =
      withProduct a $
        withProduct b $
          cont

first :: a ⊗ b -> a
first (Product a _) = a

second :: a ⊗ b -> b
second (Product _ b) = b
