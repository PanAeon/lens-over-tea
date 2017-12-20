{-# LANGUAGE TemplateHaskell #-}
module UsageGuide() where

import Control.Lens
import Data.Char
import Data.Text.Lens

data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo

-- foo = allOf (folded.text) isLower ["hello"^.packed, "goodbye"^.packed]
-- x = "hello"^.packed
-- FIXME: "hello"^.packed
-- Ambiguous type variable `s0' arising from a use of `hello'
-- NoMonomorphismRestriction !!
newtype Neither a b = Neither { _nor :: Either a b } deriving (Show)
makePrisms ''Neither
