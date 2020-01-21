{-# LANGUAGE UndecidableInstances #-}

module Quiver.Row.Sum.Match where

-- import Data.Void
import Data.Kind
import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Implicit.Param
import Quiver.Row.Entail
import Quiver.Row.Sum.Sum
import Quiver.Row.Sum.Elim
import Quiver.Row.Product.Product

class
  ( SumRow row
  , ProductRow (CoRow row)
  , SumToRow row ~ ProductToRow (CoRow row)
  )
  => CoMatch row where
    type family CoRow row :: (Type -> Type) -> Type

    withMatcher
      :: forall r1 r2
       . CoRow row (Matcher r1)
      -> (SumConstraint row Identity (Matcher r1) => r2)
      -> r2

instance CoMatch (Field k label e) where
  type CoRow (Field k label e) = Field k label e

  withMatcher
    :: forall r1 r2
     . Field k label e (Matcher r1)
    -> (ImplicitParam k label (Matcher r1 (Identity e)) => r2)
    -> r2
  withMatcher (Field (Matcher matcher)) cont =
    withParam @k @label
      (Matcher $ matcher . runIdentity)
      cont

instance CoMatch Bottom where
  type CoRow Bottom = Top

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
  => CoMatch (row1 ⊕ row2) where
    type CoRow (row1 ⊕ row2) =
      (CoRow row1 ⊗ CoRow row2)

    withMatcher
      :: forall r1 r2
       . CoRow (row1 ⊕ row2) (Matcher r1)
      -> ( ( SumConstraint row1 Identity (Matcher r1)
           , SumConstraint row2 Identity (Matcher r1)
           )
           => r2)
      -> r2
    withMatcher (Product matcher1 matcher2) cont =
      withMatcher @row1 matcher1 $
        withMatcher @row2 matcher2 $
          cont

caseOf
  :: forall k (label :: k) e r
   . (e -> r)
  -> Field k label e (Matcher r)
caseOf = Field . Matcher

type Match row1 row2 =
  ( ElimSum row1
  , CoMatch row2
  , SubRow (SumToRow row2) (SumToRow row1)
  )

match
  :: forall row1 row2 r
   . ( Match row1 row2
     )
  => row1 Identity
  -> CoRow row2 (Matcher r)
  -> r
match row matcher =
  withMatcher @row2 matcher $
    withSubRow
      @(SumToRow row2)
      @(SumToRow row1)
      @Identity
      @(Matcher r) $
        elimSum row
