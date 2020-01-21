module Quiver.Row.Sum.Dual where

import Data.Kind
import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Sum.Sum
import Quiver.Row.Product.Product

class
  ( SumRow row
  , ProductRow (CoSum row)
  , ProductToRow (CoSum row) ~ SumToRow row
  )
  => DualSum row where
    type CoSum row :: (Type -> Type) -> Type

instance DualSum Bottom where
  type CoSum Bottom = Top

instance DualSum (Field k label e) where
  type CoSum (Field k label e) = Field k label e

instance
  ( DualSum row1
  , DualSum row2
  )
  => DualSum (row1 ⊕ row2) where
    type CoSum (row1 ⊕ row2) =
      CoSum row1 ⊗ CoSum row2