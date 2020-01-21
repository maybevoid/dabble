module Quiver.Row.Sum.Intro where

import Data.Functor.Identity

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Entail
import Quiver.Implicit.Param
import Quiver.Row.Sum.Sum

newtype Inject a b = Inject
  { runInject :: b -> a }

class
  (SumRow a)
  => IntroSum a where
    introSum
      :: forall f b r
       . (Functor f)
      => (a f -> b)
      -> (SumConstraint a f (Inject b) => r)
      -> r

instance IntroSum (Field k label e) where
  introSum
    :: forall f a r
     . (Field k label e f -> a)
    -> ((ImplicitParam k label
          (Inject a (f e))
        ) => r)
    -> r
  introSum inject cont =
    withParam @k @label
      (Inject $ inject . Field)
      cont

instance
  ( IntroSum a
  , IntroSum b
  )
  => IntroSum (a ⊕ b) where
    introSum
      :: forall f c r
       . (Functor f)
      => (a ⊕ b ⋄ f -> c)
      -> (( SumConstraint a f (Inject c)
          , SumConstraint b f (Inject c)
          ) => r)
      -> r
    introSum inject cont =
      introSum (inject . Inl) $
        introSum (inject . Inr) $
          cont

type ConstructSum row a e =
  ( Row row
  , IntroSum a
  , Entails
      (SumConstraint a Identity (Inject (a Identity)))
      (SumConstraint row Identity (Inject (a Identity)))
  )

constructSum
  :: forall k (label :: k) a e
   . ( ConstructSum (Field k label e) a e)
  => e
  -> a Identity
constructSum x =
  introSum @a @Identity id $
    withEntail
      @(SumConstraint a Identity (Inject (a Identity)))
      @(SumConstraint (Field k label e) Identity (Inject (a Identity))) $
        runInject (captureParam @k @label) (Identity x)
