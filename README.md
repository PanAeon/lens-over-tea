# lens-over-tea

Just my fiddling over lens-over tea excelent blogpost

see https://artyom.me/lens-over-tea-1


A bit of history:
  1. Twan van Laarhoven Functor lenses http://www.twanvl.nl/blog/haskell/cps-functional-references
  2.  Russel O'Connor generalized lenses http://r6.ca/blog/20120623T104901Z.html
  3. Edward Kmett  mirrored lenses http://comonad.com/reader/2012/mirrored-lenses/


--------

--generalized composition
class Category cat where
  -- An “identity element” – doesn't change anything when composed.
  id :: cat a a
  -- Composition – if you replace “cat” with e.g. “~>”, its type becomes
  -- “(b ~> c) -> (a ~> b) -> (a ~> c)”.
  (.) :: cat b c -> cat a b -> cat a c



----
 TODO: part II Laws


 >>  “Essence of the Iterator Pattern”


<&> as flipped of <$>

-- TODO: check Reader/Writer state links.

-- TODO: revisit after completing more lens tutorials
-- and writing some lens code with Aeson
