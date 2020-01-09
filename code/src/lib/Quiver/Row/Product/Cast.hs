module Quiver.Row.Product.Cast where

import Data.Functor.Identity

import Quiver.Row.Entail
import Quiver.Row.Product.Intro
import Quiver.Row.Product.Product

castProduct
  :: forall a b
   . ( ProductRow a
     , IntroProduct b
     , Entails
         (ProductConstraint a Identity)
         (ProductConstraint b Identity)
     )
  => a
  -> b
castProduct x =
  withProduct x $
    withEntail
      @(ProductConstraint a Identity)
      @(ProductConstraint b Identity) $
      introProduct @b
