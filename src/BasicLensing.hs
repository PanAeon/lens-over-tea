{-# LANGUAGE TemplateHaskell #-}

module BasicLensing() where

import Control.Lens

data Arc = Arc {_degree :: Int, _minute :: Int, _second :: Int }
data Location = Location { _latitude :: Arc, _longitude :: Arc }

$(makeLenses ''Arc)
$(makeLenses ''Location)

-- actual for lens is
-- latitude :: Functor f => (Arc -> f Arc) -> Location -> f Location
-- which is Lens' Location Arc

getLatitude :: Location -> Arc
getLatitude = view latitude

setLatitude :: Arc -> Location -> Location
setLatitude = set latitude

getLatitudeR :: Location -> Arc
getLatitudeR (Location { _latitude = lat }) = lat

setLatitudeR :: Arc -> Location -> Location
setLatitudeR lat loc = loc { _latitude = lat }

l :: Lens' Location Arc
l = lens getLatitudeR (flip setLatitudeR)

modifyLatitude :: (Arc -> Arc) -> (Location -> Location)
modifyLatitude f = latitude `over` f

getDegreeOfLat :: Location -> Int
getDegreeOfLat = view degree . view latitude

setDegreeOfLat :: Int -> Location -> Location
setDegreeOfLat = over latitude . set degree

degreeOfLat :: Lens' Location Int
degreeOfLat = latitude . degree

latLong :: Lens' (Location, Location) (Arc, Arc)
latLong = latitude `alongside` longitude

degOrMin :: Lens' (Either Arc Arc) Int
degOrMin = choosing degree minute

-- holding on to a value that's focused on a constituent of a larger type

{-
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
-}
