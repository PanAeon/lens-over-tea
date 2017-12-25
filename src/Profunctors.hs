{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}


module Profunctors
    (
    ) where

import Control.Applicative
import Data.Functor.Contravariant (Contravariant (..))
import Data.Function(on)
import Control.Arrow( (***) )
import Data.Profunctor (Profunctor (dimap))
import Control.Lens.Indexed
import Data.Map as Map


newtype Predicate a = Predicate { getPredicate ∷ a → Bool }

instance Contravariant Predicate where
    contramap g (Predicate p) = Predicate (p . g)

veryOdd ∷ Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

{-
newtype Const a b = Const a
instance Contravariant (Const a) where
    contramap _ (Const a) = Const a
-}

newtype Comparison a = Comparison (a → a → Ordering) -- e.g. compare
instance Contravariant Comparison where
    contramap g (Comparison comp) = Comparison (comp `on` g)


class Bifunctor f where
    bimap ∷ (a → c) → (b → d) → f a b → f c d

instance Bifunctor Either where
  bimap f g eab = either (Left . f) (Right . g) eab

instance Bifunctor (,) where
  bimap = (***)
  --bimap f g (a,b)  = (f a, g b)


class Profunctor' f where
    dimap' ∷ (c → a) → (b → d) → f a b → f c d

lmap ∷ Profunctor f ⇒ (c → a) → f a b → f c b
lmap = (`dimap` id)

rmap ∷ Profunctor f ⇒ (b → d) → f a b → f a d
rmap = (id `dimap`)

instance Profunctor' (->) where
  dimap' g h f = h . f . g

type Limits a = Limits' a a
data Limits' a b = Limits
    { step ∷ a → (b, b)
    , check ∷ a → a → Bool }

instance Profunctor Limits' where
  dimap g h (Limits step' check') = Limits {
                     step = \c -> bimap h h (step' (g c))   -- (h *** h) . step . g
                  ,  check = \a1 a2 -> check' (g a1) (g a2) -- check `on` g
                  }

maybeLimit ∷ a → Limits a → Limits (Maybe a)
maybeLimit d = dimap (maybe d id) Just

millionsLimit ∷ Limits Double → Limits Double
millionsLimit = dimap (1.0e6 *) (/ 1.0e6)

--  Control.Lens.Indexed module provides the Indexed Profunctor:
-- newtype Indexed i a b = Indexed { runIndexed ∷ i → a → b }

mapIndexable ∷ Indexable i p ⇒ p a b → Map i a → Map i b
-- show
mapIndexable = Map.mapWithKey . indexed
