module Quiver.Row.Sum.Sum where

import Quiver.Row.Row

data Sum a b =
  Inl a | Inr b
  deriving (Eq, Show)

infixr 6 ⊕

type a ⊕ b = Sum a b

instance
  ( Row a, Row b )
  => Row (a ⊕ b) where
    type RowConstraint (Sum a b) f =
      (RowConstraint a f, RowConstraint b f)