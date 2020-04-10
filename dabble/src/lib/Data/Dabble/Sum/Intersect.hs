module Data.Dabble.Sum.Intersect where

import Data.Functor.Identity
import Data.QuasiParam.Label

import Data.Dabble.Row
import Data.Dabble.Field
import Data.Dabble.Entail
import Data.Dabble.Sum.Sum
import Data.Dabble.Sum.Dual
import Data.Dabble.Product.Product

data Merge f g e = Merge
  { firstElem :: f e
  , secondElem :: g e
  }

class
  (DualSum row1)
  => IntersectSum row1 where
    intersectSumProduct
      :: forall row2 f1 f2
       . ( ProductRow row2
         , SubRow'
            (ProductToRow row2)
            (SumToRow row1)
            f2
            Identity
         )
      => row1 f1
      -> row2 f2
      -> row1 (Merge f1 f2)

    intersectSums
      :: forall f1 f2
       . row1 f1
      -> row1 f2
      -> Maybe (row1 (Merge f1 f2))

instance IntersectSum Bottom where
  intersectSumProduct bot _ = bottom bot

  intersectSums bot _ = bottom bot

instance
  IntersectSum (Field k label e) where
    intersectSumProduct
      :: forall row f1 f2
       . ( ProductRow row
         , SubRow'
            (ProductToRow row)
            (Field k label e)
            f2
            Identity
         )
      => Field k label e f1
      -> row f2
      -> Field k label e (Merge f1 f2)
    intersectSumProduct field prod =
      Field $ Merge e1 e2
     where
      e1 :: f1 e
      (Field e1) = field

      e2 :: f2 e
      e2 =
        withProduct prod $
          withSubRow'
            @(ProductToRow row)
            @(Field k label e) $
            runIdentity $ captureParam @k @label

    intersectSums (Field e1) (Field e2) =
      Just $ Field $ Merge e1 e2

instance
  ( IntersectSum row1
  , IntersectSum row2
  )
  => IntersectSum (row1 ⊕ row2) where
    intersectSumProduct
      :: forall row3 f1 f2
       . ( ProductRow row3
         , SubRow'
            (ProductToRow row3)
            (SumToRow (row1 ⊕ row2))
            f2
            Identity
         )
      => row1 ⊕ row2 ⋄ f1
      -> row3 f2
      -> row1 ⊕ row2 ⋄ Merge f1 f2
    intersectSumProduct (Inl x) prod =
      joinSubRow
        @(ProductToRow row3)
        @(SumToRow (row1 ⊕ row2))
        @(SumToRow row1)
        @f2
        @Identity $
          Inl $ intersectSumProduct x prod

    intersectSumProduct (Inr x) prod =
          joinSubRow
            @(ProductToRow row3)
            @(SumToRow (row1 ⊕ row2))
            @(SumToRow row2)
            @f2
            @Identity $
              Inr $ intersectSumProduct x prod

    intersectSums
      :: forall f1 f2
       . row1 ⊕ row2 ⋄ f1
      -> row1 ⊕ row2 ⋄ f2
      -> Maybe (row1 ⊕ row2 ⋄ Merge f1 f2)
    intersectSums (Inl e1) (Inl e2) =
      Inl <$> intersectSums e1 e2
    intersectSums (Inr e1) (Inr e2) =
      Inr <$> intersectSums e1 e2
    intersectSums _ _ = Nothing
