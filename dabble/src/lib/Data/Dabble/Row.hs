module Data.Dabble.Row
where

import Data.Kind

class
  Row (row :: (Type -> Type) -> Type) where
    type family RowConstraint
      row (f :: Type -> Type) (t :: Type -> Type)
      = (c :: Constraint) | c -> row f t

data Union
  (row1 :: (Type -> Type) -> Type)
  (row2 :: (Type -> Type) -> Type)
  (f :: Type -> Type)

infixr 7 ∪
type row1 ∪ row2 = Union row1 row2

infixr 5 ⋄
type a ⋄ b = a b

instance
  ( Row row1
  , Row row2
  )
  => Row (Union row1 row2) where
    type RowConstraint (Union row1 row2) f t =
      ( RowConstraint row1 f t
      , RowConstraint row2 f t
      )

data Top (f :: Type -> Type) = Top

data Bottom (f :: Type -> Type)

bottom :: forall f a . Bottom f -> a
bottom bot = case bot of {}

class TopConstraint
  (f :: Type -> Type)
  (t :: Type -> Type)

instance TopConstraint f t

class BottomConstraint
  (f :: Type -> Type)
  (t :: Type -> Type)

instance BottomConstraint f t

instance Row Top where
  type RowConstraint Top f t =
    TopConstraint f t

instance Row Bottom where
  type RowConstraint Bottom f t =
    BottomConstraint f t
