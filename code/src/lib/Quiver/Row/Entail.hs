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

type RowCast' row1 row2 f t =
  (RowConstraint row1 f t => Dict (RowConstraint row2 f t))

type RowCast row1 row2 =
  (forall f t . RowCast' row1 row2 f t)

class
  ( Row row1
  , Row row2
  )
  => SubRow' row1 row2 f t where
    rowCast' :: RowCast' row1 row2 f t

instance
  ( Row row1
  , Row row2
  , p ~ RowConstraint row1 f t
  , q ~ RowConstraint row2 f t
  , p => q
  )
  => SubRow' row1 row2 f t where
    rowCast' :: RowCast' row1 row2 f t
    rowCast' = Dict

class
  (Row row1, Row row2)
  => SubRow row1 row2 where
    rowCast :: RowCast row1 row2

instance
  ( Row row1
  , Row row2
  , forall f t . SubRow' row1 row2 f t
  )
  => SubRow row1 row2 where
    rowCast :: RowCast row1 row2
    rowCast = rowCast' @row1 @row2

class
  ( Row row1
  , Row row2
  , Row row3
  , SubRow row1 row2
  , SubRow row2 row3
  , SubRow row1 row3
  )
  => JoinSubRow row1 row2 row3 where

instance
  ( Row row1
  , Row row2
  , Row row3
  , forall f t . SubRow' row1 row2 f t
  , forall f t . SubRow' row2 row3 f t
  , forall f t . SubRow' row1 row3 f t
  )
  => JoinSubRow row1 row2 row3 where

joinSubRow
  :: forall row1 row2 row3 r
   . ( JoinSubRow row1 row2 row3 )
  => (SubRow row2 row3 => r)
  -> (SubRow row1 row3 => r)
joinSubRow cont = cont

joinSubRow'
  :: forall row1 row2 row3 f t r
   . ( Row row1
     , Row row2
     , Row row3
     , SubRow' row1 row2 f t
     , SubRow' row2 row3 f t
     )
  => (SubRow' row2 row3 f t => r)
  -> (SubRow' row1 row3 f t => r)
joinSubRow' cont = cont

withRowCast
  :: forall row1 row2 f t r
   . (RowConstraint row1 f t)
  => RowCast row1 row2
  -> ((RowConstraint row2 f t) => r)
  -> r
withRowCast caster cont =
  case caster @f @t of
    Dict -> cont

withSubRow
  :: forall row1 row2 f t r
   . ( SubRow row1 row2
     , RowConstraint row1 f t
     )
  => ((RowConstraint row2 f t) => r)
  -> r
withSubRow cont =
  withRowCast @row1 @row2 @f @t
    (rowCast @row1 @row2) cont

withSubRow'
  :: forall row1 row2 f t r
   . ( SubRow' row1 row2 f t
     , RowConstraint row1 f t
     )
  => ((RowConstraint row2 f t) => r)
  -> r
withSubRow' cont =
  case (rowCast' @row1 @row2 @f @t) of
    Dict -> cont

joinEntail
  :: forall p1 p2 p3 r
   . ( Entails p1 p2
     , Entails p2 p3
     )
  => (Entails p1 p3 => r)
  -> r
joinEntail cont = cont
