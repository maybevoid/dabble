module Quiver.Row.Sum.Intro where

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Entail
import Quiver.Implicit.Param
import Quiver.Row.Sum.Sum

newtype Inject a b = Inject
  { runInject :: b -> a }

class
  (Row a)
  => IntroSum a where
    introSum
      :: forall b r
       . (a -> b)
      -> (RowConstraint a (Inject b) => r)
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
      -> (( RowConstraint a (Inject c)
          , RowConstraint b (Inject c)
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
      (RowConstraint a (Inject a))
      (RowConstraint row (Inject a))
  )

constructSum
  :: forall k (label :: k) a e
   . (ConstructSum (Field k label e) a e)
  => e
  -> a
constructSum x =
  introSum @a id $
    withEntail
      @(RowConstraint a (Inject a))
      @(RowConstraint (Field k label e) (Inject a)) $
        runInject (captureParam @k @label) x
