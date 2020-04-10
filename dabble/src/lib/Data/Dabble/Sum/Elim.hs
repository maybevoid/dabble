module Data.Dabble.Sum.Elim where

import Data.Dabble.Row
import Data.Dabble.Field
import Data.QuasiParam.Label
import Data.Dabble.Sum.Sum

newtype Matcher r a = Matcher
  { runMatcher :: a -> r }

class
  (SumRow row)
  => ElimSum row where
    elimSum
      :: forall f r
       . ( Functor f
         , SumConstraint row f (Matcher r)
         )
      => row f
      -> r

    convergeSum
      :: forall f r
       . row f
      -> (forall x . f x -> r)
      -> r

instance ElimSum Bottom where
  elimSum
    :: forall f r
     . Bottom f
    -> r
  elimSum = bottom

  convergeSum bot _ = bottom bot

instance ElimSum (Field k label e) where
  elimSum
    :: forall f r
     . (Param k label (Matcher r (f e)))
    => Field k label e f
    -> r
  elimSum (Field x) =
    runMatcher (captureParam @k @label) x

  convergeSum
    :: forall f r
     . Field k label e f
    -> (forall x . f x -> r)
    -> r
  convergeSum (Field e) f = f e

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
      => a ⊕ b ⋄ f
      -> r
    elimSum (Inl x) = elimSum x
    elimSum (Inr x) = elimSum x

    convergeSum
      :: forall f r
       . a ⊕ b ⋄ f
      -> (forall x . f x -> r)
      -> r
    convergeSum (Inl x) f =
      convergeSum x f

    convergeSum (Inr x) f =
      convergeSum x f
