-- | some utility functions for generating and manipulating a map
module Load.Map where
-- we should use only basic functions here
import Prelude()
import UPrelude
import Data ( MapTiles(..), MapTile(..), MapSettings(..), MapType(..) )

--genMapData ∷ MapSettings → MapTiles
--genMapData (MapSettings _ MapNormal (w,h)) = MapTiles (w,h) [tiles,tiles]
--  where tiles = take w $ repeat $ take h $ repeat $ MapTile 1 1
--genMapData _                               = MapTiles (10,10) [tiles,tiles]
--  where tiles = take 10 $ repeat $ take 10 $ repeat $ MapTile 1 1
