module Quiver.Row.Sum.Elim where

import Data.Void

import Quiver.Row.Field
import Quiver.Implicit.Param
import Quiver.Row.Sum.Sum

newtype Matcher r a = Matcher
  { runMatcher :: a -> r }

class
  (SumRow a)
  => ElimSum a where
    elimSum
      :: forall r
       . (SumConstraint a (Matcher r))
      => a
      -> r

instance ElimSum Void where
  elimSum
    :: forall r
     . Void
    -> r
  elimSum = absurd

instance ElimSum (Field k label e) where
  elimSum
    :: forall r
     . (ImplicitParam k label (Matcher r e))
    => Field k label e
    -> r
  elimSum (Field x) =
    runMatcher (captureParam @k @label) x

instance
  ( ElimSum a
  , ElimSum b
  )
  => ElimSum (a ⊕ b) where
    elimSum
      :: forall r
       . ( SumConstraint a (Matcher r)
         , SumConstraint b (Matcher r)
         )
      => a ⊕ b
      -> r
    elimSum (Inl x) = elimSum x
    elimSum (Inr x) = elimSum x
