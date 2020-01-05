module Quiver.Row.Product.Elim where

import GHC.Types

import Quiver.Row.Row
import Quiver.Row.Entail
import Quiver.Row.Field
import Quiver.Implicit.Param
import Quiver.Row.Product.Product

class
  (Row a)
  => ElimProduct a where
    elimGetter
      :: forall b r
       . (b -> a)
      -> (RowConstraint a ((->) b) => r)
      -> r

instance ElimProduct (Field k label e) where
  elimGetter
    :: forall a r
     . (a -> Field k label e)
    -> ((ImplicitParam k label (a -> e)) => r)
    -> r
  elimGetter getField cont =
    withParam @k @label (unField . getField) cont

instance
  ( ElimProduct a
  , ElimProduct b
  )
  => ElimProduct (a ⊗ b) where
    elimGetter
      :: forall c r
       . (c -> a ⊗ b)
      -> ( ( RowConstraint a ((->) c)
           , RowConstraint b ((->) c)
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
      (RowConstraint a ((->) a))
      (ImplicitParam k label (a -> e))
  )

type NamedGetterField name a e = GetterField Symbol name a e

optionalRow
  :: forall k (label :: k) a e r
   . (GetterField k label a (Maybe e) => r)
  -> (GetterField k label a e => r)
optionalRow cont =
  withParam @k @label getOptional cont
 where
  get :: a -> e
  get = getRow @k @label

  getOptional :: a -> Maybe e
  getOptional = Just . get

getRow
  :: forall k (label :: k) a e
   . (GetterField k (label :: k) a e)
  => a
  -> e
getRow x =
  elimGetter @a id $
    captureParam @k @label x

getNamedRow
  :: forall (label :: Symbol) a e
   . (GetterField Symbol label a e)
  => a
  -> e
getNamedRow = getRow @Symbol @label
