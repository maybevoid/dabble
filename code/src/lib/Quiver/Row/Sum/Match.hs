{-# LANGUAGE UndecidableInstances #-}

module Quiver.Row.Sum.Match where

-- import Data.Void
-- import Data.Functor.Identity

-- import Quiver.Row.Field
-- import Quiver.Implicit.Param
-- import Quiver.Row.Entail
-- import Quiver.Row.Sum.Sum
-- import Quiver.Row.Sum.Elim
-- import Quiver.Row.Product.Product

-- class
--   ( SumRow row )
--   => CoMatch row where
--     type family CoRow row r =
--       corow | corow -> row r

--     withMatcher
--       :: forall r1 r2
--        . CoRow row r1
--       -> (SumConstraint row Identity (Matcher r1) => r2)
--       -> r2

-- instance CoMatch (Field k label e) where
--   type CoRow (Field k label e) r =
--     Field k label (e -> r) Identity

--   withMatcher
--     :: forall r1 r2
--      . Field k label (e -> r1)
--     -> (ImplicitParam k label (Matcher r1 e) => r2)
--     -> r2
--   withMatcher (Field cont1) cont2 =
--     withParam @k @label (Matcher cont1) cont2

-- data EmptyMatch r = EmptyMatch

-- instance CoMatch Bottom where
--   type CoRow Bottom r = EmptyMatch r

--   withMatcher
--     :: forall r1 r2
--      . EmptyMatch r1
--     -> r2
--     -> r2
--   withMatcher EmptyMatch cont = cont

-- instance
--   ( CoMatch row1
--   , CoMatch row2
--   )
--   => CoMatch (row1 ⊕ row2) where
--     type CoRow (row1 ⊕ row2) r =
--       (CoRow row1 r ⊗ CoRow row2 r) Identity

--     withMatcher
--       :: forall r1 r2
--        . CoRow row1 r1 ⊗ CoRow row2 r1
--       -> ( ( SumConstraint row1 (Matcher r1)
--            , SumConstraint row2 (Matcher r1)
--            )
--            => r2)
--       -> r2
--     withMatcher (Product cont1 cont2) cont3 =
--       withMatcher cont1 $
--         withMatcher cont2 $
--           cont3

-- caseOf
--   :: forall k (label :: k) e r
--    . (e -> r)
--   -> Field k label (e -> r)
-- caseOf = Field

-- type Match row1 row2 =
--   ( ElimSum row1
--   , CoMatch row2
--   , SubRow (SumToRow row2) (SumToRow row1)
--   )

-- match
--   :: forall row1 row2 r
--    . ( ElimSum row1
--      , CoMatch row2
--      , SubRow (SumToRow row2) (SumToRow row1)
--      )
--   => row1
--   -> CoRow row2 r
--   -> r
-- match row matcher =
--   withMatcher matcher $
--     withSubRow
--       @(SumToRow row2)
--       @(SumToRow row1)
--       @(Matcher r) $
--       elimSum row
