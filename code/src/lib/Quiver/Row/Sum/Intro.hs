module Quiver.Row.Sum.Intro where

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
      :: forall b r
       . (a -> b)
      -> (SumConstraint a (Inject b) => r)
      -> r

instance IntroSum (Field k label e) where
  introSum
    :: forall a r
     . (Field k label e -> a)
    -> ((ImplicitParam k label
          (Inject a e)
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
      :: forall c r
       . (a ⊕ b -> c)
      -> (( SumConstraint a (Inject c)
          , SumConstraint b (Inject c)
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
      (SumConstraint a (Inject a))
      (SumConstraint row (Inject a))
  )

constructSum
  :: forall k (label :: k) a e
   . (ConstructSum (Field k label e) a e)
  => e
  -> a
constructSum x =
  introSum @a id $
    withEntail
      @(SumConstraint a (Inject a))
      @(SumConstraint (Field k label e) (Inject a)) $
        runInject (captureParam @k @label) x
