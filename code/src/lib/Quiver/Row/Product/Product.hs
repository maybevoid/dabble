module Quiver.Row.Product.Product where

import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Implicit.Param

data Product a b = Product a b
  deriving (Eq, Show)

class
  (Row row)
  => ProductRow row where
    withProduct
      :: forall r
      . row
      -> (RowConstraint row Identity => r)
      -> r

infixr 7 ⊗

type a ⊗ b = Product a b

(⊗) :: a -> b -> a ⊗ b
(⊗) = Product

instance
  ( Row a, Row b )
  => Row (a ⊗ b) where
    type RowConstraint (Product a b) f =
      (RowConstraint a f, RowConstraint b f)

instance ProductRow (Field k label e) where
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
    withProduct
      :: forall r
       . a ⊗ b
      -> (( RowConstraint a Identity
          , RowConstraint b Identity
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
