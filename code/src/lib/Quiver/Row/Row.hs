module Quiver.Row.Row
where

import GHC.Types

import Quiver.Implicit.Param

class Row row where
  type family RowConstraint row (f :: Type -> Type) :: Constraint

data EmptyRow = EmptyRow

instance Row EmptyRow where
  type RowConstraint EmptyRow f = ()

newtype Field k (label :: k) e = Field {
  unField :: e
} deriving (Eq)

type NamedField (name :: Symbol) = Field Symbol name

instance Row (Field k (label :: k) e) where
  type RowConstraint (Field k label e) f =
    ImplicitParam k label (f e)