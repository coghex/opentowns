-- | buffers contain abstract tiles, then provide functions
--   to turn them into dynamic data
module Prog.Buff where
-- a buffer of invisibly dynamic tiles to manipulate
import Prelude()
import UPrelude
import Elem.Data
import Load.Data
    ( DrawState(dsTiles, dsWins, dsBuff),
      DynData(DynData, ddTex),
      DynMap(DMNULL, DMBuff),
      Dyns(..),
      Tile(DTile, GTile) )
import Luau.Data ( Window(..), Page(..) )
import Luau.Window (currentWin)
import Vulk.Font ( TTFData(..), GlyphMetrics(..), indexTTFData )

-- | buffer dyns initiated with size n, index b
initBuff ∷ [Int] → [Dyns]
initBuff []     = []
initBuff (n:ns) = dyns : initBuff ns
  where dyns = Dyns $ take n $ repeat $ DynData (0,0) (1,1) 0 (0,0)

-- | b is the buffer index, n is the buffer
--   size, move is movability, atl is the atlas size
makeBufferTiles ∷ Int → Int → Bool → (Int,Int) → [Tile]
makeBufferTiles b n move atl
  | n ≡ 0     = []
  | otherwise = makeBufferTiles b (n - 1) move atl ⧺ [tile]
  where tile = DTile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) atl move 0

-- | loads dyns from a drawstate
loadDyns ∷ DrawState → Dyns
loadDyns ds = Dyns $ reverse $ loadDynData ds $ dsTiles ds

loadDynData ∷ DrawState → [Tile] → [DynData]
loadDynData _  []            = []
loadDynData ds (GTile {}:ts) = [] ⧺ loadDynData ds ts
loadDynData ds ((DTile (DMBuff b n) _ _ _ _ _ _):ts)
  = [buff !! n] ⧺ loadDynData ds ts
    where Dyns buff = dsBuff ds !! b
loadDynData ds ((DTile DMNULL _ _ _ _ _ _):ts)
  = [DynData (0,0) (1,1) 0 (0,0)] ⧺ loadDynData ds ts

-- | generates buffs from drawstate
genDynBuffs ∷ [TTFData] → DrawState → [Dyns]
--genDynBuffs ttfdat ds = dynsRes
genDynBuffs ttfdat ds = dynsRes
  where dyns0 = dsBuff ds
        dyns1 = case currentWin (dsWins ds) of
          Nothing → dyns0
          -- Just _ → dyns0
          -- TODO: generate dynamic buffers
          Just w → genTextDyns ttfdat w dyns0
          --Just w  → genPUTextDyns ttfdat popups
          --            (genPopupDyns popups
          --            (genTextDyns ttfdat
          --            (genButtDyns (genLinkDyns dyns0 w) w) w) w) w
        dynsRes = dyns1
--        popups = dsPopup ds

-- | set dyns in buff
setTileBuff ∷ Int → Dyns → [Dyns] → [Dyns]
setTileBuff n dyns buff = take n buff ⧺ [dyns] ⧺ tail (drop n buff)

-- | turns text from a window's page into dynamic data
genTextDyns ∷ [TTFData] → Window → [Dyns] → [Dyns]
genTextDyns ttfdat win = setTileBuff 2 dyns
  where dyns = Dyns $ newD ⧺ take (256 - length newD)
                 (repeat (DynData (0,0) (0,0) 0 (0,0)))
        newD = findPagesText ttfdat (winSize win) (winPages win)

-- | text data requires a buffer set to a 1x1 texture atlas
findPagesText ∷ [TTFData] → (Int,Int) → [Page] → [DynData]
findPagesText _      _    []     = []
findPagesText ttfdat size (p:ps) = findPageText ttfdat size p ⧺ findPagesText ttfdat size ps

-- | returns dynamic data for a single page's text
findPageText ∷ [TTFData] → (Int,Int) → Page → [DynData]
findPageText ttfdat size (Page _ elems) = findElemText ttfdat size elems

-- | text data requires a buffer set to 1x1 texture atlas
findElemText ∷ [TTFData] → (Int,Int) → [WinElem] → [DynData]
findElemText _      _    []      = []
findElemText ttfdat size ((WinElemText (x,y) _   str):wes) = dyns ⧺ findElemText ttfdat size wes
  where dyns  = calcTextDD ttfdat pos' str
        pos'  = ((2*x) - xNorm, (-2*y) + yNorm + 0.1)
        xNorm = fromIntegral(fst size)/64.0
        yNorm = fromIntegral(snd size)/64.0
findElemText ttfdat size (_:wes) = findElemText ttfdat size wes

---- functions to convert winelems to dyn data
calcTextDD ∷ [TTFData] → (Double,Double) → String → [DynData]
calcTextDD ttfdat pos = genStrDDs ttfdat (fst pos) pos
---- dyns required for a string
genStrDDs ∷ [TTFData] → Double → (Double,Double) → String → [DynData]
genStrDDs _       _  _   []         = []
genStrDDs ttfdat x0 (_,y) ('\n':str) = genStrDDs ttfdat x0 (x0,y - 1)  str
genStrDDs ttfdat x0 (x,y) (' ':str)  = genStrDDs ttfdat x0 (x + 0.1,y) str
genStrDDs ttfdat x0 (x,y) (ch:str)   = dd
  where dd = case indexTTFData ttfdat ch of
               Nothing → genStrDDs ttfdat x0 (x,y) str
               Just (TTFData _ chInd (GlyphMetrics chW chH chX chY chA))
                 → [DynData (realToFrac(x + (2*chX) + chW)
                 , realToFrac(y + (2*chY) - chH - 0.1))
                   (realToFrac chW,realToFrac chH) chInd (0,0)]
                   ⧺ genStrDDs ttfdat x0 (x + (2*chA),y) str

-- | returns a string summarising the buffer situation
printBuff ∷ DrawState → String
printBuff ds = printDyns $ dsBuff ds
-- | turns dyns into chars, hlint provides a snazzy one-liner
printDyns ∷ [Dyns] → String
--printDyns []     = []
--printDyns (d:ds) = (printDyn d) ⧺ "\n" ⧺ printDyns ds
printDyns = foldr (\ d -> (⧺) (printDyn d ⧺ "\n")) []

-- | returs a string for Dyns
printDyn ∷ Dyns → String
printDyn (Dyns d) = printDD d
printDD ∷ [DynData] → String
printDD []     = []
printDD (d:ds)
  | ddTex d ≡ 0 = printDD ds
  | otherwise   = show d ⧺ printDD ds
