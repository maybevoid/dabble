module Data.Dabble.Product.Dual where

import Data.Kind
import Data.Dabble.Row
import Data.Dabble.Field
import Data.Dabble.Sum.Sum
import Data.Dabble.Product.Product

class
  ( ProductRow row
  , SumRow (CoProduct row)
  , ProductToRow row ~ SumToRow (CoProduct row)
  )
  => DualProduct row where
    type CoProduct row :: (Type -> Type) -> Type

instance DualProduct Top where
  type CoProduct Top = Bottom

instance DualProduct (Field k label e) where
  type CoProduct (Field k label e) = Field k label e

instance
  ( DualProduct row1
  , DualProduct row2
  )
  => DualProduct (row1 ⊗ row2) where
    type CoProduct (row1 ⊗ row2) =
      CoProduct row1 ⊕ CoProduct row2
