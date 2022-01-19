module Elem.World where
-- the world map is calculated here
import Prelude()
import UPrelude
import Data ( Color(..), MapType(..), MapTiles(..), MapTile(..) )
import Elem.Data ( WinElem(..) )
import Load.Data ( DynData(..) )

genWorldDyns ∷ WinElem → [DynData]
genWorldDyns (WinElemMap mtype mtiles) = genTileDyns mtiles
genWorldDyns _ = []
genTileDyns ∷ MapTiles → [DynData]
genTileDyns (MapTiles size tiles) = flatten $ map (genTileDynsRow size) $ zip tiles [0..]
genTileDynsRow ∷ (Int,Int) → ([MapTile],Int) → [DynData]
genTileDynsRow size (row,j) = reverse $ map (genTileDynsSpot size j) $ zip row [0..]
genTileDynsSpot ∷ (Int,Int) → Int → (MapTile,Int) → DynData
genTileDynsSpot (w,_) j (MapTile _ _,i) = DynData
  (i'',j'')
  (1,1) 110 (12,0) (Color 255 255 255 255)
  where i'' = (i' + j') - (w' - 1)
        j'' = (0.5*i' - 0.5*j')
        i'  = fromIntegral i
        j'  = fromIntegral j
        w'  = fromIntegral w
--        h'  = fromIntegral h

genMapTiles ∷ MapType → MapTiles
genMapTiles MapNormal = MapTiles (10,10) tiles
  where tiles = take 10 $ repeat $ take 10 $ repeat $ MapTile 1 1
genMapTiles _ = MapTiles (0,0) [[]]
