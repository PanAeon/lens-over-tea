{-# LANGUAGE Rank2Types, TypeFamilies #-}
module LensesYoneda
    (
    ) where

type RepF f g = (Functor f, Functor g)
    => forall t . f t -> g t

type family Eta :: (* -> *) -> *

type NatF f = Functor f
    => forall g . Functor g
    => forall t. (f t -> g t) -> Eta g

type NatApA a b =
    forall g . Functor g
    => forall t. ((a -> t) -> g t) -> g b

--NatApA a b ≈ a -> b

-- Ніосіліл :( I wish tutorials were simpler 
