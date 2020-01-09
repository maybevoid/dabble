{-# LANGUAGE UndecidableInstances #-}

module Quiver.Row.Entail where

import Data.Kind
import Quiver.Row.Row
import Quiver.Row.Dict

class
  (p => q)
  => Entails (p :: Constraint) (q :: Constraint) where
    withEntail
      :: forall r
      . p
      => (q => r)
      -> r

instance (p => q)
  => Entails p q where
    withEntail cont = cont

type RowCast' row1 row2 f =
  (RowConstraint row1 f => Dict (RowConstraint row2 f))

type RowCast row1 row2 =
  (forall f . RowCast' row1 row2 f)

class
  (Row row1, Row row2)
  => SubRow' row1 row2 f where
    rowCast' :: RowCast' row1 row2 f

instance
  ( Row row1
  , Row row2
  , p ~ RowConstraint row1 f
  , q ~ RowConstraint row2 f
  , p => q
  )
  => SubRow' row1 row2 f where
    rowCast' :: RowCast' row1 row2 f
    rowCast' = Dict

class
  (Row row1, Row row2)
  => SubRow row1 row2 where
    rowCast :: RowCast row1 row2

instance
  ( Row row1
  , Row row2
  , forall f . SubRow' row1 row2 f
  )
  => SubRow row1 row2 where
    rowCast :: RowCast row1 row2
    rowCast = rowCast' @row1 @row2

withRowCast
  :: forall row1 row2 f r
   . (RowConstraint row1 f)
  => RowCast row1 row2
  -> ((RowConstraint row2 f) => r)
  -> r
withRowCast caster cont =
  case caster @f of
    Dict -> cont

withSubRow
  :: forall row1 row2 f r
   . ( SubRow row1 row2
     , RowConstraint row1 f
     )
  => ((RowConstraint row2 f) => r)
  -> r
withSubRow cont =
  withRowCast @row1 @row2 @f
    (rowCast @row1 @row2) cont

joinEntail
  :: forall p1 p2 p3 r
   . ( Entails p1 p2
     , Entails p2 p3
     )
  => (Entails p1 p3 => r)
  -> r
joinEntail cont = cont
