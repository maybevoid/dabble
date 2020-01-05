module Quiver.Row.Product.Cast where

import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Entail
import Quiver.Row.Product.Intro
import Quiver.Row.Product.Product

castProduct
  :: forall a b
   . ( ProductRow a
     , IntroProduct b
     , Entails
         (RowConstraint a Identity)
         (RowConstraint b Identity)
     )
  => a
  -> b
castProduct x =
  withProduct x $
    withEntail
      @(RowConstraint a Identity)
      @(RowConstraint b Identity) $
      introProduct @b
