module Quiver.Row.Field
where

import GHC.Types

import Quiver.Implicit.Param

import Quiver.Row.Row

newtype Field k (label :: k) e = Field {
  unField :: e
} deriving (Eq, Show)

type NamedField (name :: Symbol) = Field Symbol name

instance Row (Field k (label :: k) e) where
  type RowConstraint (Field k label e) f =
    ImplicitParam k label (f e)