module Data.Dabble.Product.Cast where

import Data.Functor.Identity

import Data.Dabble.Entail
import Data.Dabble.Product.Intro
import Data.Dabble.Product.Product

castProduct
  :: forall f a b
   . ( ProductRow a
     , IntroProduct b
     , Entails
         (ProductConstraint a f Identity)
         (ProductConstraint b f Identity)
     )
  => a f
  -> b f
castProduct x =
  withProduct x $
    withEntail
      @(ProductConstraint a f Identity)
      @(ProductConstraint b f Identity) $
      introProduct @b
