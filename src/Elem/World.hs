module Elem.World where
-- TODO: get rid of all of this
-- the world map is calculated here
import Prelude()
import UPrelude
import Data.List ( zip7 )
import Data ( Color(..), MapType(..), MapTiles(..), Space, Plane, Row
            , MapTile(..), MapSettings(..), Cards(..) )
import Elem.Data ( WinElem(..) )
import Load.Data ( DynData(..) )

-- | generate dyn list from world data
genWorldDyns ∷ WinElem → [DynData]
genWorldDyns (WinElemMap mtype mtiles) = genTileDyns mtiles
genWorldDyns _ = []
genTileDyns ∷ MapTiles → [DynData]
genTileDyns (MapTiles size tiles) = flatten $ map (genTileDynsZ size) $ zip tiles' [0..]
  where tiles' = trimTiles size tiles
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

-- | only tiles on top and in front need rendering
trimTiles ∷ (Int, Int) → Space MapTile → Space MapTile
trimTiles (x, y) mts = map trimPlane $ zip mts cards
  where cards = genCards x y mts
trimPlane ∷ (Plane MapTile, Plane (Cards MapTile)) → Plane MapTile
trimPlane (p,c) = map trimRow $ zip p c
trimRow ∷ (Row MapTile, Row (Cards MapTile)) → Row MapTile
trimRow (r,c) = map trimSpot $ zip r c
trimSpot ∷ (MapTile, Cards MapTile) → MapTile
trimSpot (t,Cards3D _ Nothing _       _ _ _       ) = t
trimSpot (t,Cards3D _ _       Nothing _ _ _       ) = t
trimSpot (_,Cards3D _ _       _       _ _ (Just _)) = MapTile 0 0
trimSpot (t,_)                                      = t

-- | given a 3d space return a new 3d array of cardinal points
genCards ∷ Int → Int → Space MapTile → Space (Cards MapTile)
genCards x y mts = map genCardsPlanes $ zip7 mts mtsAbove mtsBelow
                                             mtsNorth mtsSouth
                                             mtsEast mtsWest
  where blankPlane = take x $ repeat blankRow
        mtsBelow   = tail mts ⧺ [blankPlane]
        mtsAbove   = [blankPlane] ⧺ init mts
        mtsNorth   = map nCards mts
        nCards p   = map ncRows p
        ncRows r   = tail r ⧺ [blankTile]
        mtsSouth   = map sCards mts
        sCards p   = map scRows p
        scRows r   = [blankTile] ⧺ init r
        mtsEast    = map eCards mts
        eCards p   = tail p ⧺ [blankRow]
        mtsWest    = map wCards mts
        wCards p   = [blankRow] ⧺ init p
        blankRow   = take y $ repeat blankTile
        blankTile  = MapTile 0 0
genCardsPlanes ∷ (Plane MapTile,Plane MapTile
                 ,Plane MapTile,Plane MapTile
                 ,Plane MapTile,Plane MapTile
                 ,Plane MapTile) → Plane (Cards MapTile)
genCardsPlanes (p,pA,pB,pN,pS,pE,pW)
  = map genCardsRows $ zip7 p pA pB pN pS pE pW
genCardsRows ∷ (Row MapTile,Row MapTile
               ,Row MapTile,Row MapTile
               ,Row MapTile,Row MapTile
               ,Row MapTile) → Row (Cards MapTile)
genCardsRows (r,rA,rB,rN,rS,rE,rW)
  = map genCardsRow $ zip7 r rA rB rN rS rE rW
genCardsRow ∷ (MapTile,MapTile,MapTile
              ,MapTile,MapTile,MapTile
              ,MapTile) → Cards MapTile
genCardsRow (_,tA,tB,tN,tS,tE,tW) = Cards3D (san tN) (san tS)
                                            (san tE) (san tW)
                                            (san tA) (san tB)
  where san t = case t of
                MapTile 0 0 → Nothing
                MapTile a b → Just $ MapTile a b

-- | this is just a test set of data for now
genMapTiles ∷ MapSettings → MapTiles
genMapTiles (MapSettings _ MapNormal _) = MapTiles (j,k) [tiles j k 1 1]
  where tiles x y i c = take x $ repeat $ take y $ repeat $ MapTile i c
        (j,k)         = (40,40)
        testbuff      = take 9  $ repeat $ take 10 $ repeat $ MapTile 1 0
        testrow       = take 9  $ repeat $ MapTile 0 0
        testspot      = MapTile 2 2
        testlevel     = (testspot : testrow) : testbuff
        t2            = t2buff ⧺ [t2row,t2row,t2row,t2row]
        t2buff        = take 6  $ repeat $ take 10 $ repeat $ MapTile 0 0
        t2row         = take 10 $ repeat $ MapTile 2 2
        t3            = t3buff ⧺ [t3row,t3row,t3row]
        t3buff        = take 7  $ repeat $ take 10 $ repeat $ MapTile 0 0
        t3row         = t3rowbuff ⧺ [testspot,testspot,testspot]
        t3rowbuff     = take 7  $ repeat $ MapTile 0 0
genMapTiles _ = MapTiles (0,0) [[[]]]

-- | basic printer
printMap ∷ (Show α) ⇒ Space α → IO ()
printMap = printMapF 0
printMapF ∷ (Show α) ⇒ Int → Space α → IO ()
printMapF _ []     = return ()
printMapF n (p:ps) = do
  print $ "z(" ⧺ show n ⧺ "):" ⧺ show p
  printMapF (n+1) ps
showTiles ∷ (Show α) ⇒ Space α → String
showTiles = showTilesF 0
showTilesF ∷ (Show α) ⇒ Int → Space α → String
showTilesF _ []     = []
showTilesF n (p:ps) = show p ⧺ "\n" ⧺ showTilesF (n+1) ps
