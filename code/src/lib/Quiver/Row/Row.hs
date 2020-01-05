module Quiver.Row.Row
where

import Data.Kind

class Row row where
  type family RowConstraint
    row (f :: Type -> Type)
    :: Constraint

instance Row () where
  type RowConstraint () f = ()
