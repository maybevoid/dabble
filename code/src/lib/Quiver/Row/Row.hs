module Quiver.Row.Row
where

import Data.Kind
import Data.Void

class Row row where
  type family RowConstraint
    row (f :: Type -> Type)
    :: Constraint

instance Row () where
  type RowConstraint () f = ()

instance Row Void where
  type RowConstraint Void f = ()