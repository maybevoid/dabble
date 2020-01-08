module Quiver.Row.Sum.Match where

import Quiver.Row.Row
import Quiver.Row.Field
import Quiver.Implicit.Param
import Quiver.Row.Entail
import Quiver.Row.Sum.Sum
import Quiver.Row.Sum.Elim
import Quiver.Row.Product.Product

class
  (Row (MatchRow a))
  => Match a where
    type family MatchRow a
    type family Return a

    withMatcher
      :: forall r
       . a
      -> ((RowConstraint (MatchRow a) (Matcher (Return a)))
          => r)
      -> r

instance Match (Field k label (e -> r)) where
  type MatchRow (Field k label (e -> r)) = (Field k label e)
  type Return (Field k label (e -> r)) = r

  withMatcher
    :: forall r2
     . Field k label (e -> r)
    -> ((ImplicitParam k label (Matcher r e))
        => r2)
    -> r2
  withMatcher (Field cont1) cont2 =
    withParam @k @label (Matcher cont1) cont2

instance
  ( Match a
  , Match b
  , Return a ~ Return b
  )
  => Match (a ⊗ b) where
    type MatchRow (a ⊗ b) =
      ( MatchRow a ⊗ MatchRow b )

    type Return (a ⊗ b) = Return a

    withMatcher
      :: forall r
       . a ⊗ b
      -> ((RowConstraint (MatchRow a) (Matcher (Return a))
          , RowConstraint (MatchRow b) (Matcher (Return b))
          )
          => r)
      -> r
    withMatcher (Product cont1 cont2) cont3 =
      withMatcher cont1 $
        withMatcher cont2 $
          cont3

caseOf
  :: forall k (label :: k) e r
   . (e -> r)
  -> Field k label (e -> r)
caseOf = Field

match
  :: forall row1 row2
   . ( ElimSum row1
     , Match row2
     , Entails
        (RowConstraint (MatchRow row2) (Matcher (Return row2)))
        (RowConstraint row1 (Matcher (Return row2)))
     )
  => row1
  -> row2
  -> Return row2
match row matcher =
  withMatcher matcher $
    withEntail
      @(RowConstraint (MatchRow row2) (Matcher (Return row2)))
      @(RowConstraint row1 (Matcher (Return row2))) $
      elimSum row


class (Row row) => Matchable row where
  type family ToMatchRow row r

instance Matchable (Field k label e) where
  type ToMatchRow (Field k label e) r =
    Field k label (e -> r)

instance
  ( Matchable a
  , Matchable b
  )
  => Matchable (a ⊕ b) where
    type ToMatchRow (Sum a b) r =
      (ToMatchRow a r) ⊗ (ToMatchRow b r)

type MatchField row1 row2 r =
  ( ElimSum row1
  , Matchable row2
  , Match (ToMatchRow row2 r)
  , Return (ToMatchRow row2 r) ~ r
  , Entails
      (RowConstraint row2 (Matcher r))
      (RowConstraint row1 (Matcher r))
  )
