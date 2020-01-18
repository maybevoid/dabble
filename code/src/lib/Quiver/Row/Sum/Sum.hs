module Quiver.Row.Sum.Sum where

import Data.Kind
import Data.Void

import Quiver.Row.Row
import Quiver.Row.Field

data Sum a b (f :: Type -> Type)
  = Inl (a f)
  | Inr (b f)
  deriving (Eq, Show)

infixr 6 ⊕

type a ⊕ b = Sum a b

type SumConstraint row f t =
  RowConstraint (SumToRow row f) t

class SumRow
  (row :: (Type -> Type) -> Type)
  where
    type family SumToRow row (f :: Type -> Type)

data Bottom (f :: Type -> Type)

instance SumRow Bottom where
  type SumToRow Bottom f = ()

instance SumRow (Field k label e) where
  type SumToRow (Field k label e) f = Field k label e f

instance
  ( SumRow a, SumRow b )
  => SumRow (a ⊕ b) where
    type SumToRow (a ⊕ b) f =
      (SumToRow a f) ∪ (SumToRow b f)
