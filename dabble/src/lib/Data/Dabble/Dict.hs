{-# LANGUAGE PolyKinds #-}

module Data.Dabble.Dict where

import Data.QuasiParam.Label

data Dict p where
  Dict :: p => Dict p

mergeDict :: Dict p -> Dict q -> Dict (p, q)
mergeDict Dict Dict = Dict

implicitDict
  :: forall k (label :: k) a
   . a
  -> Dict (Param k label a)
implicitDict x =
  withParam @k @label x Dict
