module Quiver.Row.Sum.Partition where

import Data.Void

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Row.Entail
import Quiver.Row.Sum.Sum
import Quiver.Row.Sum.Elim
import Quiver.Row.Sum.Match

class PartitionSum a b1 b2 where
  partitionSum
    :: a
    -> Either b1 b2

instance
  PartitionSum
    (Field k label e)
    (Field k label e)
    Void
  where
    partitionSum = Left

instance
  PartitionSum
    (Field k label e)
    Void
    (Field k label e)
  where
    partitionSum = Right

instance
  (PartitionSum a b1 b2)
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
  (PartitionSum a b1 b2)
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

openMatch
  :: forall row1 row11 row12 row2
   . ( PartitionSum row1 row11 row12
     , ElimSum row11
     , Match row2
     , Entails
        (RowConstraint (MatchRow row2) (Matcher (Return row2)))
        (RowConstraint row11 (Matcher (Return row2)))
     )
  => row1
  -> row2
  -> (row12 -> Return row2)
  -> Return row2
openMatch row1 row2 defaultCase =
  case partitionSum @row1 @row11 @row12 row1 of
    Left row11 ->
      match row11 row2
    Right row12 ->
      defaultCase row12