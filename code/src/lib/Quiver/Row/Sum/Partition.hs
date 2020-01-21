
module Quiver.Row.Sum.Partition where

import Data.Functor.Identity
import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Sum.Sum
import Quiver.Row.Sum.Match
import Quiver.Row.Sum.Elim

class
  ( SumRow a
  , SumRow b1
  , SumRow b2
  ) =>
  PartitionSum a b1 b2 where
    partitionSum
      :: forall f
      . a f
      -> Either (b1 f) (b2 f)

instance
  {-# INCOHERENT #-}
  ( SumRow a
  , SumRow b
  ) =>
  PartitionSum (a ⊕ b) a b where
    partitionSum (Inl x) = Left x
    partitionSum (Inr x) = Right x

instance
  {-# INCOHERENT #-}
  ( SumRow a
  , SumRow b
  ) =>
  PartitionSum (a ⊕ b) b a where
    partitionSum (Inl x) = Right x
    partitionSum (Inr x) = Left x

instance
  {-# INCOHERENT #-}
  PartitionSum
    (Field k label e)
    (Field k label e)
    Bottom
  where
    partitionSum = Left

instance
  {-# INCOHERENT #-}
  PartitionSum
    (Field k label e)
    Bottom
    (Field k label e)
  where
    partitionSum = Right

instance
  {-# INCOHERENT #-}
  ( SumRow c
  , PartitionSum a b1 b2
  )
  => PartitionSum
      (c ⊕ a)
      (c ⊕ b1)
      b2
  where
    partitionSum (Inl c) = Left $ Inl c
    partitionSum (Inr a) =
      case partitionSum a of
        Left b1 -> Left $ Inr b1
        Right b2 -> Right b2

instance
  {-# INCOHERENT #-}
  ( SumRow c
  , PartitionSum a b1 b2
  )
  => PartitionSum
      (c ⊕ a)
      b1
      (c ⊕ b2)
  where
    partitionSum (Inl c) = Right $ Inl c
    partitionSum (Inr a) =
      case partitionSum a of
        Left b1 -> Left b1
        Right b2 -> Right $ Inr b2

type OpenMatch row11 row12 row1 row2 =
  ( PartitionSum row1 row11 row12
  , Match row11 row2
  )

openMatch
  :: forall row11 row12 row1 row2 r
   . ( OpenMatch row11 row12 row1 row2 )
  => row1 Identity
  -> CoRow row2 (Matcher r)
  -> (row12 Identity -> r)
  -> r
openMatch row1 row2 defaultCase =
  case partitionSum @row1 @row11 @row12 row1 of
    Left row11 ->
      match @_ @row2 row11 row2
    Right row12 ->
      defaultCase row12
