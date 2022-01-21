-- | some utility functions for generating and manipulating a map
module Load.Map where
-- we should use only basic functions here
import Prelude()
import UPrelude
import Data ( MapTiles(..), MapTile(..) )

genMapData ∷ (Int,Int) → MapTiles
genMapData (w,h) = MapTiles (w,h) [tiles,tiles]
  where tiles = take w $ repeat $ take h $ repeat $ MapTile 1 1
