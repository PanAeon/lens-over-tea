{-# LANGUAGE TemplateHaskell #-}
module LilLensStarter
    (
    ) where

import Control.Lens
import Test.QuickCheck
-- lens laws
-- 1. Get-Put  - get a then put a -- s stays the same
-- 2. Put-Get  - insert a then you should get a
-- 3. Put-Put  - if you put a, then b you should get b


-- basic operators:

-- view  :: Control.Monad.Reader.Class.MonadReader s m => Getting a s a -> m a
-- (^.) :: s -> Getting a s a -> a (infix view) ^? is view for prisms, toListOf for traversals or ^..
-- set .~
-- over %~


-- Get-Put
getPut :: (String, String) -> Bool
getPut whole = set _1 (view _1 whole) whole == whole

getPut' = quickCheck getPut

putGet :: Int -> (String, String) -> Bool
putGet x whole = view _1 (set _1 x whole) == x

putGet' = quickCheck putGet

putPut :: Int -> String -> (String, String) -> Bool
putPut a b whole = (set _1 b (set _1 a whole)) == (set _1 b whole)

putPut' = quickCheck putPut


-- TODO: check lens-aeson (not library, should be built-in)

type Degrees = Double
type Longitude = Degrees
type Latitude = Degrees

data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude)} deriving Show
makeLenses ''Meetup -- :info name

meetupLat ::Lens' Meetup Latitude
meetupLat = location . _1

meetupLon ::Lens' Meetup Longitude
meetupLon = location . _2

z = Meetup "foo" (3.4, 2.5) ^. location . _1


-- (_1 .~ 3) ("1", ())  ==> 1,2) & _1 .~ 3

-- Prism are like lens for a sum type, like for Either[...,...]
-- Prisms are dual of lenses
-- They select just one branch to go down.

fj=preview _Left (Left "hi")
jf=review _Left "hi"
sdf=preview _Cons [1,2,3]

-- Traversals are Lenses which focus on multiple targets simultaneously.

items :: Traversable t =>  Traversal' (t a) a
items = traverse

z' = [1,2,3] ^.. items

-- Isos are isomorphisms, connections between types that are equivalent in every way.

-- someIso :: Iso' (Maybe a) (Either () a)
-- someIso = _ TODO: write some Iso's 
--
    -- _ :: p (Either () a) (f (Either () a)) -> p (Maybe a) (f (Maybe a))
