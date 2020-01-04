module Quiver.Row.Product.Product where

import Quiver.Row.Row

data Product a b = Product a b

instance
  ( Row a, Row b )
  => Row (Product a b) where
    type RowConstraint (Product a b) f =
      (RowConstraint a f, RowConstraint b f)

first :: Product a b -> a
first (Product a _) = a

second :: Product a b -> b
second (Product _ b) = b
