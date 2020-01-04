{-# LANGUAGE PolyKinds #-}

module Quiver.Row.Dict where

import Quiver.Implicit.Param

data Dict p where
  Dict :: p => Dict p

mergeDict :: Dict p -> Dict q -> Dict (p, q)
mergeDict Dict Dict = Dict

implicitDict
  :: forall (label :: k) a
   . a
  -> Dict (ImplicitParam k label a)
implicitDict x =
  withParam @k @label x Dict
