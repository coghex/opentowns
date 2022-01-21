module Elem.World where
-- TODO: get rid of all of this
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
genTileDyns (MapTiles size tiles) = flatten $ map (genTileDynsZ size) $ zip tiles [0..]
genTileDynsZ ∷ (Int,Int) → ([[MapTile]],Int) → [DynData]
genTileDynsZ size (zlvl,l) = flatten $ map (genTileDynsRow size l) $ zip zlvl [0..]
genTileDynsRow ∷ (Int,Int) → Int → ([MapTile],Int) → [DynData]
genTileDynsRow size l (row,j) = reverse $ map (genTileDynsSpot size l j) $ zip row [0..]
genTileDynsSpot ∷ (Int,Int) → Int → Int → (MapTile,Int) → DynData
genTileDynsSpot (_,_) _ _ (MapTile 0 _,_)
  = DynData (0,0) (1,1) 0 (0,0) (Color 0 0 0 0)
genTileDynsSpot (w,_) l j (MapTile t _,i) = DynData
  (i'',j'')
  (1,1) 110 (indexTerrain t) (Color 255 255 255 255)
  where i'' = (i' + j') - (w' - 1)
        j'' = (0.5*i' - 0.5*j') + l'
        i'  = fromIntegral i
        j'  = fromIntegral j
        w'  = fromIntegral w
        l'  = fromIntegral l
--        h'  = fromIntegral h

indexTerrain ∷ Int → (Int,Int)
indexTerrain 1 = (12,0)
indexTerrain 2 = (14,0)
indexTerrain _ = (0,0)

genMapTiles ∷ MapType → MapTiles
genMapTiles MapNormal = MapTiles (10,10) [tiles 1 1, testlevel]
  where tiles i c = take 10 $ repeat $ take 10 $ repeat $ MapTile i c
        testbuff  = take 9 $ repeat $ take 10 $ repeat $ MapTile 0 0
        testrow   = take 9 $ repeat $ MapTile 0 0
        testspot  = MapTile 2 2
        testlevel = (testspot : testrow) : testbuff
genMapTiles _ = MapTiles (0,0) [[[]]]
