module Quiver.Row.Sum.Sum where

import Data.Void
import Quiver.Row.Row
import Quiver.Row.Field

data Sum a b =
  Inl a | Inr b
  deriving (Eq, Show)

infixr 6 ⊕

type a ⊕ b = Sum a b

type SumConstraint row f =
  RowConstraint (SumToRow row) f

class
  (Row (SumToRow row))
  => SumRow row where
    type family SumToRow row

instance SumRow Void where
  type SumToRow Void = ()

instance SumRow (Field k label e) where
  type SumToRow (Field k label e) = Field k label e

instance
  ( SumRow a, SumRow b )
  => SumRow (a ⊕ b) where
    type SumToRow (a ⊕ b) =
      (SumToRow a) ∪ (SumToRow b)
