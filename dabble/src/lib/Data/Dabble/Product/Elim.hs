module Data.Dabble.Product.Elim where

import GHC.Types
import Data.Functor.Identity
import Data.QuasiParam.Label

import Data.Dabble.Row
import Data.Dabble.Entail
import Data.Dabble.Field
import Data.Dabble.Product.Product

class
  (ProductRow a)
  => ElimProduct a where
    elimGetter
      :: forall f b r
       . (Functor f)
      => (b -> a f)
      -> (ProductConstraint a f ((->) b) => r)
      -> r

instance ElimProduct (Field k label e) where
  elimGetter
    :: forall f a r
     . (Functor f)
    => (a -> Field k label e f)
    -> ((Param k label (a -> f e)) => r)
    -> r
  elimGetter getField cont =
    withParam @k @label (unField . getField) cont

instance
  ( ElimProduct a
  , ElimProduct b
  )
  => ElimProduct (a ⊗ b) where
    elimGetter
      :: forall f c r
       . (Functor f)
      => (c -> a ⊗ b ⋄ f)
      -> ( ( ProductConstraint a f ((->) c)
           , ProductConstraint b f ((->) c)
           ) => r
         )
      -> r
    elimGetter getField cont =
      elimGetter (first . getField) $
        elimGetter (second . getField) $
          cont

type GetterField k (label :: k) a e =
  ( ElimProduct a
  , Entails
      (ProductConstraint a Identity ((->) (a Identity)))
      (Param k label (a Identity -> Identity e))
  )

type NamedGetterField name a e = GetterField Symbol name a e

optionalRow
  :: forall k (label :: k) a e r
   . (GetterField k label a (Maybe e) => r)
  -> (GetterField k label a e => r)
optionalRow cont =
  withParam @k @label getOptional cont
 where
  get :: a Identity -> e
  get = getRow @k @label

  getOptional :: a Identity -> Identity (Maybe e)
  getOptional = Identity . Just . get

getRow
  :: forall k (label :: k) a e
   . ( GetterField k (label :: k) a e )
  => a Identity
  -> e
getRow x =
  runIdentity $
    elimGetter @a @Identity id $
      captureParam @k @label x

getNamedRow
  :: forall (label :: Symbol) a e
   . ( GetterField Symbol label a e )
  => a Identity
  -> e
getNamedRow = getRow @Symbol @label
