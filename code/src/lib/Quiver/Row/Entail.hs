{-# LANGUAGE UndecidableInstances #-}

module Quiver.Row.Entail where

import Data.Kind

class (p => q)
  => Entails (p :: Constraint) (q :: Constraint) where
    withEntail
      :: forall r
      . p
      => (q => r)
      -> r

instance (p => q)
  => Entails p q where
    withEntail cont = cont

joinEntail
  :: forall p1 p2 p3 r
   . ( Entails p1 p2
     , Entails p2 p3
     )
  => (Entails p1 p3 => r)
  -> r
joinEntail cont = cont