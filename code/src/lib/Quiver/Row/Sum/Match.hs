{-# LANGUAGE UndecidableInstances #-}

module Quiver.Row.Sum.Match where

import GHC.Types
import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Implicit.Param
import Quiver.Row.Entail
import Quiver.Row.Sum.Sum
import Quiver.Row.Sum.Elim
import Quiver.Row.Sum.Dual
import Quiver.Row.Product.Product

class
  ( ProductRow row )
  => CoMatch row where
    withMatcher
      :: forall r1 r2
       . row (Matcher r1)
      -> (ProductConstraint row Identity (Matcher r1) => r2)
      -> r2

instance CoMatch (Field k label e) where
  withMatcher
    :: forall r1 r2
     . Field k label e (Matcher r1)
    -> (ImplicitParam k label (Matcher r1 (Identity e)) => r2)
    -> r2
  withMatcher (Field (Matcher matcher)) cont =
    withParam @k @label
      (Matcher $ matcher . runIdentity)
      cont

instance CoMatch Top where
  withMatcher
    :: forall r1 r2
     . Top (Matcher r1)
    -> r2
    -> r2
  withMatcher Top cont = cont

instance
  ( CoMatch row1
  , CoMatch row2
  )
  => CoMatch (row1 ⊗ row2) where
    withMatcher
      :: forall r1 r2
       . (row1 ⊗ row2) (Matcher r1)
      -> ( ( ProductConstraint row1 Identity (Matcher r1)
           , ProductConstraint row2 Identity (Matcher r1)
           )
           => r2)
      -> r2
    withMatcher (Product matcher1 matcher2) cont =
      withMatcher @row1 matcher1 $
        withMatcher @row2 matcher2 $
          cont

caseOf
  :: forall (label :: Symbol) e r
   . (e -> r)
  -> NamedField label e (Matcher r)
caseOf = Field . Matcher

type Match row1 row2 =
  ( ElimSum row1
  , DualSum row2
  , CoMatch (CoSum row2)
  , SubRow (SumToRow row2) (SumToRow row1)
  )

match
  :: forall row1 row2 r
   . ( ElimSum row1
     , CoMatch row2
     , SubRow (ProductToRow row2) (SumToRow row1)
     )
  => row1 Identity
  -> row2 (Matcher r)
  -> r
match row matcher =
  withMatcher @row2 matcher $
    withSubRow
      @(ProductToRow row2)
      @(SumToRow row1)
      @Identity
      @(Matcher r) $
        elimSum row
