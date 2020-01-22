module Quiver.Row.Sum.Match where

import GHC.Types
import Data.Functor.Identity

import Quiver.Row.Field
import Quiver.Row.Entail
import Quiver.Row.Sum.Sum
import Quiver.Row.Sum.Elim
import Quiver.Row.Sum.Dual
import Quiver.Row.Sum.Partition
import Quiver.Row.Sum.Intersect
import Quiver.Row.Product.Product


caseOf
  :: forall (label :: Symbol) e r
   . (e -> r)
  -> NamedField label e (Matcher r)
caseOf = Field . Matcher

type Match row1 row2 =
  ( ElimSum row1
  , DualSum row2
  , IntersectSum row1
  , SubRow
      (SumToRow row2)
      (SumToRow row1)
  )

type OpenMatch row11 row12 row1 row2 =
  ( PartitionSum row1 row11 row12
  , Match row11 row2
  )

match
  :: forall row1 row2 r
   . ( ElimSum row1
     , IntersectSum row1
     , ProductRow row2
     , SubRow
        (ProductToRow row2)
        (SumToRow row1)
     )
  => row1 Identity
  -> row2 (Matcher r)
  -> r
match row1 matcher = convergeSum row2 mergeMatch
 where
  row2 :: row1 (Merge Identity (Matcher r))
  row2 =
    runSubRow
      @(ProductToRow row2)
      @(SumToRow row1)
      @(Matcher r)
      @Identity $
        intersectSumProduct row1 matcher

  mergeMatch
    :: forall x
     . Merge Identity (Matcher r) x
    -> r
  mergeMatch
    (Merge (Identity x) (Matcher cont))
    = cont x

openMatch
  :: forall row11 row12 row1 row2 r
   . ( PartitionSum row1 row11 row12
     , ElimSum row11
     , IntersectSum row11
     , ProductRow row2
     , SubRow
        (ProductToRow row2)
        (SumToRow row11)
     )
  => row1 Identity
  -> row2 (Matcher r)
  -> (row12 Identity -> r)
  -> r
openMatch row1 row2 defaultCase =
  case partitionSum @row1 @row11 @row12 row1 of
    Left row11 ->
      match @_ @row2 row11 row2
    Right row12 ->
      defaultCase row12
