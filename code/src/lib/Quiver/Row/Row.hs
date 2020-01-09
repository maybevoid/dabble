module Quiver.Row.Row
where

import Data.Kind

class Row row where
  type family RowConstraint
    row (f :: Type -> Type)
    = (c :: Constraint) | c -> row f

data Union row1 row2

infixr 7 ∪
type row1 ∪ row2 = Union row1 row2

instance
  ( Row row1
  , Row row2
  )
  => Row (Union row1 row2) where
    type RowConstraint (Union row1 row2) f =
      ( RowConstraint row1 f
      , RowConstraint row2 f
      )

class NoConstraint (f :: Type -> Type)

instance NoConstraint f

instance Row () where
  type RowConstraint () f =
    NoConstraint f
