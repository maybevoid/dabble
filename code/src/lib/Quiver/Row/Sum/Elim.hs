module Quiver.Row.Sum.Elim where

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Implicit.Param
import Quiver.Row.Sum.Sum

newtype Matcher r a = Matcher
  { runMatcher :: a -> r }

class
  (SumRow a)
  => ElimSum a where
    elimSum
      :: forall f r
       . ( Functor f
         , SumConstraint a f (Matcher r)
         )
      => a f
      -> r

instance ElimSum Bottom where
  elimSum
    :: forall f r
     . Bottom f
    -> r
  elimSum = bottom

instance ElimSum (Field k label e) where
  elimSum
    :: forall f r
     . (ImplicitParam k label (Matcher r (f e)))
    => Field k label e f
    -> r
  elimSum (Field x) =
    runMatcher (captureParam @k @label) x

instance
  ( ElimSum a
  , ElimSum b
  )
  => ElimSum (a ⊕ b) where
    elimSum
      :: forall f r
       . ( Functor f
         , SumConstraint a f (Matcher r)
         , SumConstraint b f (Matcher r)
         )
      => (a ⊕ b) f
      -> r
    elimSum (Inl x) = elimSum x
    elimSum (Inr x) = elimSum x
