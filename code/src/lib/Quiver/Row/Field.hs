module Quiver.Row.Field
where

import GHC.Types

import Quiver.Implicit.Param

import Quiver.Row.Row

newtype Field k (label :: k) e f = Field {
  unField :: f e
} deriving (Eq, Show)

type NamedField (name :: Symbol) = Field Symbol name

instance Row (Field k (label :: k) e f) where
  type RowConstraint (Field k label e f) t =
    ImplicitParam k label (t (f e))