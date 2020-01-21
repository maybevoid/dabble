module Quiver.Row.Sum.Sum where

import Data.Kind

import Quiver.Row.Row
import Quiver.Row.Field

data Sum a b (f :: Type -> Type)
  = Inl (a f)
  | Inr (b f)
  deriving (Eq, Show)

infixr 6 ⊕

type a ⊕ b = Sum a b

type SumConstraint row f t =
  RowConstraint (SumToRow row) f t

class SumRow
  (row :: (Type -> Type) -> Type)
  where
    type family SumToRow row
      :: (Type -> Type) -> Type

instance SumRow Bottom where
  type SumToRow Bottom = Top

instance SumRow (Field k label e) where
  type SumToRow (Field k label e) = Field k label e

instance
  ( SumRow a, SumRow b )
  => SumRow (a ⊕ b) where
    type SumToRow (a ⊕ b) =
      (SumToRow a) ∪ (SumToRow b)
