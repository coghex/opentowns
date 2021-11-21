-- | buffers contain abstract tiles, then provide functions
--   to turn them into dynamic data
module Prog.Buff where
-- a buffer of invisibly dynamic tiles to manipulate
import Prelude()
import UPrelude
import Data ( Color(..), Difficulty(..) )
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
  where dyns = Dyns $ take n $ repeat $ DynData (0,0) (1,1) 0 (0,0) (Color 1 1 1 0)

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
  = [DynData (0,0) (1,1) 0 (0,0) (Color 1 1 1 0)] ⧺ loadDynData ds ts

-- | generates buffs from drawstate
genDynBuffs ∷ [TTFData] → DrawState → [Dyns]
--genDynBuffs ttfdat ds = dynsRes
genDynBuffs ttfdat ds = dynsRes
  where dyns0   = dsBuff ds
        dynsRes = case currentWin (dsWins ds) of
          Nothing → dyns0
          -- Just _ → dyns0
          -- TODO: generate dynamic buffers
          Just w → dyns2
            where dyns2 = genButtDyns dyns1 w
                  dyns1 = genTextDyns ttfdat w dyns0
          --Just w  → genPUTextDyns ttfdat popups
          --            (genPopupDyns popups
          --            (genTextDyns ttfdat
          --            (genButtDyns (genLinkDyns dyns0 w) w) w) w) w
--        popups = dsPopup ds

-- | set dyns in buff
setTileBuff ∷ Int → Dyns → [Dyns] → [Dyns]
setTileBuff n dyns buff = take n buff ⧺ [dyns] ⧺ tail (drop n buff)

-- generate dynamic data for a button
genButtDyns ∷ [Dyns] → Window → [Dyns]
genButtDyns buff win = setTileBuff 1 dyns buff
  where dyns = Dyns $ newD ⧺ take (64 - length newD) (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 1 1 1 0)))
        newD = findPageElemData (winSize win) (winCurr win) (winPages win)

findPageElemData ∷ (Int,Int) → String → [Page] → [DynData]
findPageElemData _    _   []     = []
findPageElemData size str ((Page name elems):ps)
  | str ≡ name = findElemData size elems ⧺ findPageElemData size str ps
  | otherwise  = findPageElemData size str ps

findElemData ∷ (Int,Int) → [WinElem] → [DynData]
findElemData _    []         = []
findElemData size ((WinElemButt (x,y) col (w,_) _ _ _ _ hov):wes) = case hov of
  True  → [dyn] ⧺ findElemData size wes
            where dyn     = DynData pos' box' 107 (4,2) col
                  (x',y') = (realToFrac x, realToFrac y)
                  w'      = realToFrac w
                  pos'    = ((2*x') - xNorm + w', (-2*y') + yNorm + 0.1)
                  xNorm   = fromIntegral(fst size)/64.0
                  yNorm   = fromIntegral(snd size)/64.0
                  box'    = (w', 0.25)
  False → findElemData size wes
findElemData size (_:wes) = findElemData size wes

-- | turns text from a window's page into dynamic data
genTextDyns ∷ [TTFData] → Window → [Dyns] → [Dyns]
genTextDyns ttfdat win = setTileBuff 2 dyns
  where dyns = Dyns $ newD ⧺ take (512 - length newD)
                 (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 1 1 1 0)))
        newD = findPagesText ttfdat (winSize win) (winCurr win) (winPages win)

-- | text data requires a buffer set to a 1x1 texture atlas
findPagesText ∷ [TTFData] → (Int,Int) → String → [Page] → [DynData]
findPagesText _      _    _       []     = []
findPagesText ttfdat size current (p:ps)
  = findPageText ttfdat size current p
  ⧺ findPagesText ttfdat size current ps

-- | returns dynamic data for a single page's text
findPageText ∷ [TTFData] → (Int,Int) → String → Page → [DynData]
findPageText ttfdat size current (Page name elems)
  | current ≡ name = findElemText ttfdat size elems
  | otherwise      = []

-- | text data requires a buffer set to 1x1 texture atlas
findElemText ∷ [TTFData] → (Int,Int) → [WinElem] → [DynData]
findElemText _      _    []      = []
findElemText ttfdat size ((WinElemText (x,y) c   str):wes) = dyns ⧺ findElemText ttfdat size wes
  where dyns  = calcTextDD c ttfdat pos' str
        pos'  = ((2*x) - xNorm, (-2*y) + yNorm + 0.1)
        xNorm = fromIntegral(fst size)/64.0
        yNorm = fromIntegral(snd size)/64.0
findElemText ttfdat size ((WinElemButt (x,y) c _ _ act _ str _):wes) = dyns ⧺ findElemText ttfdat size wes
  where dyns  = calcTextDD c ttfdat pos' str'
        pos'  = ((2*x) - xNorm, (-2*y) + yNorm + 0.1)
        xNorm = fromIntegral(fst size)/64.0
        yNorm = fromIntegral(snd size)/64.0
        str'  = calcButtString act str
findElemText ttfdat size (_:wes) = findElemText ttfdat size wes

-- | some buttons have context specific text
calcButtString ∷ ButtAction → String → String
calcButtString (ButtActionText tb) str = str ⧺ ": " ⧺ showTB tb
calcButtString _                    str = str
-- | takes a textbutton's value and makes it a string
showTB ∷ TextButton → String
showTB (TextMusic          b) = showBool b
showTB (TextMusicVolume    v) = show v ⧺ "%"
showTB (TextFX             b) = showBool b
showTB (TextFXVolume       v) = show v ⧺ "%"
showTB (TextMouseScroll    b) = showBool b
showTB (TextScrollHover    b) = showBool b
showTB (TextHeightCubes    b) = showBool b
showTB (TextItemDisableDef b) = showBool b
showTB (TextPauseOnStart   b) = showBool b
showTB (TextAutosave       v) = showDay v
showTB (TextSieges         v) = showDifficulty v
showTB (TextPauseOnSiege   b) = showBool b
showTB (TextPauseOnCaravan b) = showBool b
showTB (TextAllowBury      b) = showBool b
showTB (TextUnknown        v) = "UNK:" ⧺ show v
showTB TextNULL               = "NULL"
-- | represents bools as strings
showBool ∷ Bool → String
showBool True  = "ON"
showBool False = "OFF"
-- | specific to towns
showDay ∷ Maybe Int → String
showDay Nothing  = "Disabled"
showDay (Just 1) = "1 Day"
showDay (Just n) = show n ⧺ " Days"
showDifficulty ∷ Difficulty → String
showDifficulty DNormal   = "Normal"
showDifficulty DHard     = "Hard"
showDifficulty DHarder   = "Harder"
showDifficulty DInsane   = "Insane"
showDifficulty DDisabled = "Disabled"
showDifficulty DEasy     = "Easy"
showDifficulty DNULL     = "NULL"

-- | functions to convert winelems to dyn data
calcTextDD ∷ Color → [TTFData] → (Double,Double) → String → [DynData]
calcTextDD col ttfdat pos = genStrDDs col ttfdat (fst pos) pos
-- | dyns required for a string
genStrDDs ∷ Color → [TTFData] → Double → (Double,Double) → String → [DynData]
genStrDDs _   _       _  _   []         = []
genStrDDs col ttfdat x0 (_,y) ('\n':str) = genStrDDs col ttfdat x0 (x0,y - 1)  str
genStrDDs col ttfdat x0 (x,y) (' ':str)  = genStrDDs col ttfdat x0 (x + 0.1,y) str
genStrDDs col ttfdat x0 (x,y) (ch:str)   = dd
  where dd = case indexTTFData ttfdat ch of
               Nothing → genStrDDs col ttfdat x0 (x,y) str
               Just (TTFData _ chInd (GlyphMetrics chW chH chX chY chA))
                 → [DynData (realToFrac(x + (2*chX) + chW)
                 , realToFrac(y + (2*chY) - chH - 0.1))
                   (realToFrac chW,realToFrac chH) chInd (0,0) col]
                   ⧺ genStrDDs col ttfdat x0 (x + (2*chA),y) str

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
