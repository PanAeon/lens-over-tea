{-# LANGUAGE RankNTypes, TupleSections, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}


module LenseOverTea () where

import Data.Bifunctor
import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Product
import Control.Arrow( (***), (&&&))
import Control.Monad( join)
import Data.Monoid(getFirst, First(..), Any(..), getAny, Endo(..), appEndo)
import qualified Data.Traversable as T
import qualified Control.Category as Cat
import Control.Monad.Reader
import Data.Functor.Contravariant
import  Unsafe.Coerce

-- A lens allows us to do something to a big structure given that we know how to do something to a part of it.

-- type Lens s a = forall f . Functor f => (a -> f a) -> s -> (f s)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> (f t)

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Setting s t a b = (a -> Identity b) -> s -> Identity t


-- Storey x f a is conceptually the same as (x, f a).
data Storey x f a = Storey x (f a) deriving Show
type Storey' x f = Compose ((,) x) f

type Storey'' x f =  Product (Const x) f

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)


newtype Const' a b = Const' { getConst' :: a }

instance Functor (Const' m) where
  fmap _ (Const' m) = (Const' m)


type Traversal s t a b = forall f . Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

-- FIXME: completely lost here
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

instance T.Traversable t => Each (t a) (t b) a b where
  each = T.traverse

-- Int ->  Monad m => (a -> m a) -> [a] -> (a, m [a])

ix :: Int -> Lens [a] [a] a a
ix i f ys
   | i < 0        = error "ix: negative index"
   | null ys      = error "ix: index too large"
   | (x:xs) <- ys = if i == 0
                    then (: xs) <$> (f x)
                    else  (x :) <$> ix (i - 1) f xs

kdfj = getCompose $ ix 2 (\x -> Compose (x, [1..x])) [300, 100, 4, 600, 900, 400]
kask = runIdentity $ ix 2 (\x -> Identity 88) [1,4,5]

-- over :: Lens s t a b -> ((a -> b) -> s -> t)
-- over l f = runIdentity . l (Identity . f)

--  over (ix 2) (const 88) [0..4]

-- view :: Lens s t a b -> s -> a
-- view l s = getConst' $ l Const' s

--type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> (f t)
-- (a -> f b) -> s -> (f t)
-- view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
-- view l s = getConst $ l Const s
--
-- over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
-- over l f = runIdentity . l (Identity . f)

{-
Getting r s a is a function which, given some way to get r from a, will go over as in some s and return their combined rs.

-}

-- FIXME: check how we get this, from this:
{-
view :: Getting a s a -> s -> a
view l = getConst . l Const
-}
view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst . l Const )

-- FIXME: read this again
views :: MonadReader s m => Getting r s a -> (a -> r) -> m r
views l f = view (l . to f)

over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- set :: Lens s t a b -> b -> s -> t
-- set l x = runIdentity . l (Identity . (const x))

set :: Setting s t a b -> b -> s -> t
set l x = runIdentity . l (Identity . (const x))


-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (,x) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x,) <$> f a


-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> set s <$> (f (get s))

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = (\f es ->
                    case es of
                      (Left s1) -> Left <$> l1 f s1
                      (Right s2) -> Right <$>  l2 f s2
                 )


-- type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> (f t)
-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)--let (fb, t) = getCompose $ l (\x -> let r = (f x) in Compose (Identity r, Identity r) ) s
              --in (runIdentity fb, runIdentity t)




-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = let (fb, t) = getCompose $ l (\x -> let r = (f x) in Compose (Identity x, Identity r) ) s
               in (runIdentity fb, runIdentity t)

-- (bimap <$> id <*> id) (
-- &&&
-- join (***) Identity for (a,a) -> (f a, f a)
(<%~~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~~) l f s = bimap runIdentity runIdentity $ getCompose $ l (Compose . (Identity &&& Identity) . f ) s


-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united f s = (const s) <$> (f ())

-- Traversals 101
-- type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s
_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0     = error "_abs: absolute value can't be negative"
      | otherwise = signum n * x

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
  where
    get s     = ref
    set s new = map (\old -> if old == ref then new else old) s





_all' :: Eq a => a -> Traversal' [a] a
_all' ref f s = traverse update s
  where
    update old = if old == ref then f old else pure old



-- view :: (a -> Const a a) -> s -> Const a s-> s -> a
-- view l s = getConst $ l Const s

--toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a] -- Don't follow here
toListOf :: Getting (Endo [a]) s a -> s -> [a]   -- this is kind of important
toListOf l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))


-- (^..) :: Getting (Endo [a]) s a -> s  -> [a]
(^..) :: s -> Getting (Endo [a]) s a -> [a]
(^..) = flip toListOf

-- data First a = First (Maybe a)
--
-- instance Monoid (First a) where
--   mempty = First Nothing
--   mappend (First Nothing) y = y
--   mappend x               _ = x

-- preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

-- has :: ((a -> Const (Any) a) -> s -> Const (Any) s) -> s ->  Bool
has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (const (Const (Any True)))


data AppendList a = JustList [a] | Append (AppendList a) (AppendList a)
doAppends (Append (JustList x) y) = x ++ doAppends y
doAppends (Append (Append a b) y) = doAppends (Append a (Append b y))

instance Monoid (AppendList a) where
  mempty = JustList []
  mappend = Append

_head :: Traversal' [a] a
_head f []     = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs

--------------- some operators ---------------

-- view flipped
(^.) ::  s -> Getting a s a -> a
(^.) = flip view

(%~)  :: Setting s t a b -> (a -> b) -> s -> t
(%~) = over

--------------- composing --------------------

-- data OldLens s a = OldLens
--   { get    :: s -> a
--   , modify :: (a -> a) -> s -> s }
--
-- (@.) :: OldLens b c -> OldLens a b -> OldLens a c
-- (@.) _c _b = OldLens
--   { get    = get    _c . get    _b
--   , modify = modify _b . modify _c }
--
-- instance Cat.Category OldLens where
--   id = OldLens id id
--   (.) = (@.)

 -- l1::(c -> f c) -> b -> f b
 -- l2::(b -> f b) -> a -> f a
 -- res:: (c -> f c) -> a -> f a
-- type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s
(@.) :: Lens' b c -> Lens' a b -> Lens' a c
(@.) = flip (.)

foo :: Lens' b c -> Lens' a b -> Lens' a c
foo _c _b = \f -> _b (_c f)

(&) = flip (.)

-- view' (_1' . ix' 3 . ix' 1)
-- view' = view . ($ id)

type Getter s a = forall f . (Contravariant f, Functor f) => (a -> f a) -> s -> f s

type Fold   s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

newtype Folding f a = Folding { getFolding :: f a }

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding ( pure $ unsafeCoerce ()) -- FIXME: woot?
  mappend (Folding fa) (Folding fb) = Folding (fa *> fb)


-- coerce :: f a -> f (t a)
-- coerce = undefined
-- ("hello","world")^.to snd
to :: (s -> a) -> Getter s a
to getter f s =  unsafeCoerce (f (getter s))
--  (s -> a) -> f a -> f s  contramap
-- (s->a) -> (a -> f a) -> s -> f s
--  to getter f s = Const (getConst (f (getter s)))

folded :: (Contravariant f, Applicative f, Foldable t) =>
          (a -> f a) -> t a -> f (t a)
folded f s = unsafeCoerce . getFolding $ foldMap (Folding . f) s--contramap (undefined) (foldMap f s)

bothF :: Fold (a, a) a
bothF f s = unsafeCoerce . getFolding $ foldMap (Folding . f) [fst s, snd s]

replicated :: Int -> Fold a a
replicated n f s = unsafeCoerce $ sequenceA (replicate n (f s))

foldCombine :: Fold s a -> Fold s a -> Fold s a
foldCombine fa fb = \l s -> fa l s *> fb l s

type Iso s t a b =
  forall k f. (Isomorphic k, Functor f) => k (a -> f b) (s -> f t)

class Isomorphic k where
  isomorphic :: (a -> b) -> (b -> a) -> k a b

instance Isomorphic (->) where
  -- we just don't need the other direction
  isomorphic f _ = f

data Isomorphism a b = Isomorphism (a -> b) (b -> a)

instance Isomorphic Isomorphism where
  isomorphic = Isomorphism

from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a

isos :: (s -> a) -> (a -> s)      -- s <-> a
     -> (t -> b) -> (b -> t)      -- t <-> b
     -> Iso s t a b
isos sa as tb bt = isomorphic
  (\afb s -> bt <$> afb (sa s))   -- easy peasy
  (\sft a -> tb <$> sft (as a))   -- lemon squeezy
