module Data.Dabble.Field
where

import GHC.Types

import Data.QuasiParam.Label

import Data.Dabble.Row

newtype Field k (label :: k) e f = Field {
  unField :: f e
} deriving (Eq, Show)

type NamedField (name :: Symbol) = Field Symbol name

instance Row (Field k (label :: k) e) where
  type RowConstraint (Field k label e) f t =
    Param k label (t (f e))
