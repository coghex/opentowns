-- | buffers contain abstract tiles, then provide functions
--   to turn them into dynamic data
module Prog.Buff where
-- a buffer of invisibly dynamic tiles to manipulate
import Prelude()
import UPrelude
import Data.Map (Map)
import qualified Data.Map as Map
import Data ( Color(..), Difficulty(..), KeyFunc(..), Key(..)
            , Popup(..), PopupType(..), LoadState(..) )
import Elem.Data
import Elem.World ( genWorldDyns )
import Load.Data
    ( DrawState(..),
      DynData(DynData, ddTex),
      DynMap(DMNULL, DMBuff), Buff(..),
      Dyns(..), BuffIndex(..),
      WinsState(..),
      Tile(DTile, GTile) )
import Luau.Data ( Window(..), Page(..) )
import Luau.Window (currentWin)
import qualified Load.Stack as S
import Vulk.Font ( TTFData(..), GlyphMetrics(..), indexTTFData )

-- | buffer dyns initiated with size n, index b
initBuff ∷ [Int] → [Dyns]
initBuff []     = []
initBuff (n:ns) = dyns : initBuff ns
  where dyns = Dyns $ take n $ repeat $ DynData (0,0) (1,1) 0 (0,0) (Color 0 0 0 0)

-- | b is the buffer index, n is the buffer
--   size, move is movability, atl is the atlas size
makeBufferTiles ∷ BuffIndex → Int → Bool → (Int,Int) → [Tile]
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
  = [dyns !! n] ⧺ loadDynData ds ts
    where Dyns dyns = buff Map.! b
          Buff buff = dsBuff ds
loadDynData ds ((DTile DMNULL _ _ _ _ _ _):ts)
  = [DynData (0,0) (1,1) 0 (0,0) (Color 0 0 0 0)] ⧺ loadDynData ds ts

-- | generates buffs from drawstate, if loading is set will
--   only draw a loading screen, if game hasnt been loaded yet
--   will only draw ui elements and popups
genDynBuffs ∷ [TTFData] → DrawState → Buff
--genDynBuffs ttfdat ds = dynsRes
genDynBuffs ttfdat ds = case loading (dsWinsState ds) of
  Loading s → loadingScreen ttfdat s (dsBuff ds) 
  Unloaded  → dynsRes0
  Loaded    → dynsRes
  where dyns0   = dsBuff ds
        ws      = dsWinsState ds
        dynsRes0 = case currentWin (dsWins ds) (dsWinsState ds) of
          Nothing → dynserr
            where dynserr = genErrDyns ttfdat dyns0
          Just w  → dyns4
            where dyns4 = genPUTextDyns ttfdat (dsPopup ds) dyns3
                  dyns3 = genPopupDyns (dsPopup ds) dyns2
                  dyns2 = genButtDyns dyns1 w ws
                  dyns1 = genTextDyns ttfdat w ws dyns0
        dynsRes = case currentWin (dsWins ds) (dsWinsState ds) of
          Nothing → dynserr
            where dynserr = genErrDyns ttfdat dyns0
          -- TODO: generate dynamic buffers
          Just w → dyns4
            where dyns4 = genMapDyns dyns3 w
                  dyns3 = genPUTextDyns ttfdat (dsPopup ds) dyns2
                  dyns2 = genPopupDyns (dsPopup ds) dyns1
                  dyns1 = clearLoadingScreenDyns dyns0
          --Just w  → genPUTextDyns ttfdat popups
          --            (genPopupDyns popups
          --            (genTextDyns ttfdat
          --            (genButtDyns (genLinkDyns dyns0 w) w) w) w) w
--        popups = dsPopup ds

-- | generates a loading screen and clears all dyns
loadingScreen ∷ [TTFData] → String → Buff → Buff
loadingScreen ttfdat str dyns = dyns1
  where dyns1 = addLoadingScreen ttfdat str dyns0
        dyns0 = clearAllDyns dyns
-- | clears all dyns in a brute force way
clearAllDyns ∷ Buff → Buff
clearAllDyns (Buff buff) = Buff $ Map.map clearDyns buff
clearDyns ∷ Dyns → Dyns
clearDyns (Dyns dyns) = Dyns $ clearDDs dyns
clearDDs ∷ [DynData] → [DynData]
clearDDs []     = []
clearDDs (_:ds) = [d0] ⧺ clearDDs ds
  where d0 = DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)
-- | generic loading screen
addLoadingScreen ∷ [TTFData] → String → Buff → Buff
addLoadingScreen ttfdat str buffs = setTileBuff BuffLoadScreen dyns buffs
  where dyns = Dyns $ newD ⧺ take (ddsSize - length newD)
                 (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD       = loadLogo ⧺ msg
        loadLogo
          = [DynData (0,-4) (8,2) 109 (0,0) (Color 255 255 255 255)]
        msg
          = calcTextDD (Color 255 255 255 255) ttfdat (0,-6) str
        Dyns dds  = buff Map.! BuffLoadScreen
        Buff buff = buffs
        ddsSize   = length dds
-- | clears only the loading screen
clearLoadingScreenDyns ∷ Buff → Buff
clearLoadingScreenDyns = setTileBuff BuffLoadScreen dyns
  where dyns = Dyns $ take 32
                 (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))

-- | generates dynamic data for any number of popups, popups
--   index to the center of the screen, still in openGL-style
genPopupDyns ∷ [Popup] → Buff → Buff
genPopupDyns popups buffs = setTileBuff BuffPopup dyns buffs
  where dyns      = Dyns $ newD ⧺ take (ddsSize - length newD)
                      (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD      = genPopupDynsF popups
        Dyns dds  = buff Map.! BuffPopup
        Buff buff = buffs
        ddsSize   = length dds
genPopupDynsF ∷ [Popup] → [DynData]
genPopupDynsF = foldr ((⧺) . genEachPopupDyns) []
--genPopupDynsF []       = []
--genPopupDynsF (pu:pus) = genEachPopupDyns pu ⧺ genPopupDynsF pus
genEachPopupDyns ∷ Popup → [DynData]
genEachPopupDyns (Popup (x,y) (w,h) (PopupSavename _)) = dd
  where dd = genericPopup x y w h
genEachPopupDyns (Popup (x,y) (w,h) PopupSetKey {}) = dd
  where dd = genericPopup x y w h
genEachPopupDyns _ = []
genericPopup ∷ Double → Double → Double → Double → [DynData]
genericPopup x y w h = topleft ⧺ toprightbk ⧺ topright ⧺ bottomleft
                    ⧺ bottomright ⧺ top ⧺ bottom ⧺ right ⧺ left
                    ⧺ fill ⧺ textBoxTL ⧺ textBoxTR ⧺ textBoxBL
                    ⧺ textBoxBR ⧺ textBoxTop ⧺ textBoxBot ⧺ textBoxR
                    ⧺ textBoxL ⧺ textBoxFill ⧺ checkbox
  where topleft
          = [DynData (x',y'+(2*h')) (0.5,0.5) 107 (0,29)
              (Color 255 255 255 255)]
        -- lol, you can see why they overwrote this tile, it does not tile right
        toprightbk   = [DynData
          (x' + (2*w'),  y' + (2*h'))   (0.5,       0.5)       107 (2,29)
          $ Color 255 255 255 255]
        topright     = [DynData
          (x' + (2*w'),  y' + (2*h'))   (0.5,       0.5)       107 (26,25)
          $ Color 255 255 255 255]
        bottomleft   = [DynData
          (x',           y')            (0.5,       0.5)       107 (0,30)
          $ Color 255 255 255 255]
        bottomright  = [DynData
          (x' + (2*w'),  y')            (0.5,       0.5)       107 (2,30)
          $ Color 255 255 255 255]
        top          = [DynData
          (x' + w',      y' + (2*h'))   (w' - 0.5,  0.5)       107 (4,29)
          $ Color 255 255 255 255]
        bottom       = [DynData
          (x' + w',      y')            (w' - 0.5,  0.5)       107 (4,30)
          $ Color 255 255 255 255]
        right        = [DynData
          (x' + (2*w'),  y' + h')       (0.5,       h' - 0.5)  107 (8,29)
          $ Color 255 255 255 255]
        left         = [DynData
          (x',           y' + h')       (0.5,       h' - 0.5)  107 (6,29)
          $ Color 255 255 255 255]
        fill         = [DynData
          (x' + w',      y' + h')       (w' - 0.5,  h' - 0.5)  107 (10,29)
          $ Color 255 255 255 255]
        textBoxTL    = [DynData
          (x'',          y'' + (2*h'')) (0.5,       0.5)       108 (8,2)
          $ Color 255 255 255 255]
        textBoxTR    = [DynData
          (x'' + (2*w''),y'' + (2*h'')) (0.5,       0.5)       108 (12,2)
          $ Color 255 255 255 255]
        textBoxBL    = [DynData
          (x'',          y'')           (0.5,       0.5)       108 (8,4)
          $ Color 255 255 255 255]
        textBoxBR    = [DynData
          (x'' + (2*w''),y'')           (0.5,       0.5)       108 (12,4)
          $ Color 255 255 255 255]
        textBoxTop   = [DynData
          (x'' + w'',    y'' + (2*h'')) (w'' - 0.5, 0.5)       108 (10,2)
          $ Color 255 255 255 255]
        textBoxBot   = [DynData
          (x'' + w'',    y'')           (w'' - 0.5, 0.5)       108 (10,4)
          $ Color 255 255 255 255]
        textBoxR     = [DynData
          (x'' + (2*w''),y'' + h'')     (0.5,       h'' - 0.5) 108 (12,3)
          $ Color 255 255 255 255]
        textBoxL     = [DynData
          (x'',          y'' + h'')     (0.5,       h'' - 0.5) 108 (8,3)
          $ Color 255 255 255 255]
        textBoxFill  = [DynData
          (x'' + w'',    y'' + h'')     (w'' - 0.5, h'' - 0.5) 108 (14,2)
          $ Color 255 255 255 255]
        checkbox = [DynData (x0 - 0.25,y0 - 2) (0.25,0.25) 108 (10,11)
                            (Color 255 255 255 255)
                   ,DynData (x0 + 0.25,y0 - 2) (0.25,0.25) 108 (11,11)
                            (Color 255 255 255 255)
                   ,DynData (x0 - 0.25,y0 - 2.5) (0.25,0.25) 108 (10,12)
                            (Color 255 255 255 255)
                   ,DynData (x0 + 0.25,y0 - 2.5) (0.25,0.25) 108 (11,12)
                            (Color 255 255 255 255)]
        -- 0 values are a basic cast
        (x0,y0) = (realToFrac x, realToFrac y)
        -- prime values locate the outer box
        (x',y') = (2*x0 - w',-2*y0 - h')
        (w',h') = (0.5*realToFrac w, 0.5*realToFrac h)
        -- double prime is the location of the inner box
        (x'',y'') = (x' + 1.0, y' + 3.0)
        (w'',h'') = (w' - 1.0, 1.0)
        

-- | generates dynamic buffer for a map
genMapDyns ∷ Buff → Window → Buff
genMapDyns buffs w = setTileBuff BuffMap dyns buffs
  where dyns = Dyns $ newD ⧺ take (ddsSize - length newD) (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD = genPageMapDyns (winPages w)
        Dyns dds  = buff Map.! BuffMap
        Buff buff = buffs
        ddsSize   = length dds
genPageMapDyns ∷ [Page] → [DynData]
genPageMapDyns []           = []
genPageMapDyns (page:pages) = pe0 ⧺ genPageMapDyns pages
  where pe0 = genElemMapDyns (pageElems page)
genElemMapDyns ∷ [WinElem] → [DynData]
genElemMapDyns []       = []
genElemMapDyns (we:wes) = pe0 ⧺ genElemMapDyns wes
  where pe0 = case we of
                WinElemMap mtype tiles → genWorldDyns we
                _                      → []

-- | generates dynamic data for the text of a popup.  in a seperate
--   buffer since the atlas format is different
genPUTextDyns ∷ [TTFData] → [Popup] → Buff → Buff
genPUTextDyns ttfdat popups buffs = setTileBuff BuffPUText dyns buffs
  where dyns = Dyns $ newD ⧺ take (ddsSize - length newD) (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD = genPUTextDynsF ttfdat popups
        Dyns dds  = buff Map.! BuffPUText
        Buff buff = buffs
        ddsSize   = length dds
genPUTextDynsF ∷ [TTFData] → [Popup] → [DynData]
genPUTextDynsF _      []       = []
genPUTextDynsF ttfdat (pu:pus) = genEachPUTextDyns ttfdat pu ⧺ genPUTextDynsF ttfdat pus

genEachPUTextDyns ∷ [TTFData] → Popup → [DynData]
genEachPUTextDyns ttfdat (Popup (x,y) _ (PopupSavename str))
  = dd ⧺ undersc
    where dd       = calcTextDD (Color 255 255 255 255) ttfdat (x,y + 3.0) "Set a savegame name"
          undersc  = calcTextDD (Color 255 255 255 255) ttfdat (x,y) $ "_" ⧺ str
genEachPUTextDyns ttfdat (Popup (x,y) _ (PopupSetKey keyInd keyFunc key))
  = dd ⧺ undersc
    where dd      = calcTextDD (Color 255 255 255 255) ttfdat (x - 6.0,y + 3.0) $ "Set " ⧺ printKeyInd keyInd ⧺ " hotkey for " ⧺ printKeyFunc keyFunc ⧺ " (Current: " ⧺ printKeys key ⧺ ")"
          undersc = calcTextDD (Color 255 255 255 255) ttfdat (x,y) "_"
genEachPUTextDyns _      _ = []
printKeyInd ∷ Int → String
printKeyInd 1 = "1st"
printKeyInd 2 = "2nd"
printKeyInd _ = "NULL"
printKeyFunc ∷ KeyFunc → String
printKeyFunc keyFunc = tail $ tail $ show keyFunc
printKeys ∷ [Key] → String
printKeys []     = []
printKeys [k]    = printKey k
printKeys (k:ks) = printKey k ⧺ ", " ⧺ printKeys ks
printKey ∷ Key → String
printKey key = tail $ tail $ tail $ show key

-- | set dyns in buff
setTileBuff ∷ BuffIndex → Dyns → Buff → Buff
setTileBuff ind dyns (Buff buff) = Buff $ Map.adjust (return dyns) ind buff
--setTileBuff n dyns buff = take n buff ⧺ [dyns] ⧺ tail (drop n buff)

-- | generate dynamic data for a button
genButtDyns ∷ Buff → Window → WinsState → Buff
genButtDyns buffs win ws = setTileBuff BuffButt dyns buffs
  where dyns = Dyns $ newD ⧺ take (ddsSize - length newD)
                 (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD = findPageElemData (winSize win) currPage (winPages win)
        Dyns dds  = buff Map.! BuffButt
        Buff buff = buffs
        ddsSize   = length dds
        ((_,currPage),_) = S.popSS $ winStack ws

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
genTextDyns ∷ [TTFData] → Window → WinsState → Buff → Buff
  -- there should be a haskell extension to allow currying here
genTextDyns ttfdat win ws buffs = setTileBuff BuffText dyns buffs
  where dyns = Dyns $ newD ⧺ take (ddsSize - length newD)
                 (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD = findPagesText ttfdat (winSize win) currPage (winPages win)
        Dyns dds  = buff Map.! BuffText
        Buff buff = buffs
        ddsSize   = length dds
        ((_,currPage),_) = S.popSS $ winStack ws

-- | some error text as a window
genErrDyns ∷ [TTFData] → Buff → Buff
genErrDyns ttfdat buffs = setTileBuff BuffText dyns buffs
  where dyns = Dyns $ newD ⧺ take (ddsSize - length newD)
                 (repeat (DynData (0,0) (0,0) 0 (0,0) (Color 0 0 0 0)))
        newD  = calcTextDD (Color 255 255 255 255) ttfdat pos' "blop"
        pos'  = (0,0)
        Dyns dds  = buff Map.! BuffText
        Buff buff = buffs
        ddsSize   = length dds

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
calcButtString _                   str = str
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
showTB (TextCPULevel       v) = show v
showTB (TextKeyMap        km) = showKeyMap km
showTB (TextUnknown        v) = "UNK:" ⧺ show v
showTB TextNULL               = "NULL"
-- | represents bools as strings
showBool ∷ Bool → String
showBool True  = "ON"
showBool False = "OFF"
-- | string representation of a key mapping
showKeyMap ∷ (KeyFunc,[Key]) → String
showKeyMap (_,keys) = "(" ⧺ showKeys keys
showKeys ∷ [Key] → String
showKeys []     = "(None)"
showKeys [k]    = show k ⧺ ")"
showKeys (k:ks) = show k ⧺ "," ⧺ showKeys ks
--showKeys = foldr (\ k -> (⧺) (show k ⧺ ",")) "]"
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
printDyns ∷ Buff → String
--printDyns []     = []
--printDyns (d:ds) = (printDyn d) ⧺ "\n" ⧺ printDyns ds
printDyns _ = "i dont know hot to print!" --foldr (\ d -> (⧺) (printDyn d ⧺ "\n")) []

-- | returs a string for Dyns
printDyn ∷ Dyns → String
printDyn (Dyns d) = printDD d
printDD ∷ [DynData] → String
printDD []     = []
printDD (d:ds)
  | ddTex d ≡ 0 = printDD ds
  | otherwise   = show d ⧺ printDD ds
