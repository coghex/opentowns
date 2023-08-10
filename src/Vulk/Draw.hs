-- | draw is the middle layer translation between an abstract idea of
--   a hypothetical window, to a giant list of 2D tiles.  these still
--   need to be converted into verticies/indicies in Vulk.Calc
module Vulk.Draw where
-- convert epiklesis state to draw state
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Data ( Color(..) )
import qualified Data.Map as Map
import Elem ( loadWindow )
import Load.Data ( DrawState(..), Tile(..), DynMap(..)
                 , BuffIndex(..), Buff(..), Dyns(..) )
import Prog.Buff ( makeBufferTiles, makeBufferTiles' )
import Vulk.Font
    ( indexTTFData, GlyphMetrics(GlyphMetrics), TTFData(TTFData) )
import Luau.Window (currentWin)

-- | loads tiles from drawstate
loadTiles ∷ DrawState → (Double,Double) → [TTFData] → [Tile]
loadTiles ds winSize ttfdat
-- TODO: move these background tiles into their own buffer
  = [GTile (0,0) (w,h) (0,0) (1,1) 105 (Color 255 255 255 255)
  ,  GTile (0,0) (8,3) (0,0) (1,1) 106 (Color 255 255 255 255)]
  ⧺ winTiles
  where winTiles   = case currentWin (dsWins ds) (dsWinsState ds) of
          Just win → linkbuff ⧺ buttbuff ⧺ popupbuff ⧺ putextbuff
                     ⧺ textbuff ⧺ mapbuff ⧺ loadbuff
                     ⧺ loadWindow nDefTex win ttfdat
          Nothing  → []
        nDefTex    = 0 -- dsNDefTex ds
        (w,h)      = winSize
        -- TODO: move the size check into the makebuffertiles function
        Buff buff  = dsBuff ds
        Dyns lbds  = buff Map.! BuffLink
        linkbuff   = makeBufferTiles BuffLink
                       (length lbds) False (32,32)
        Dyns bbds  = buff Map.! BuffButt
        buttbuff   = makeBufferTiles BuffButt
                       (length bbds) False (32,32)
        Dyns tbds  = buff Map.! BuffButt
        textbuff   = makeBufferTiles BuffText
                       (length tbds) False (1,1)
        Dyns pubds = buff Map.! BuffPopup
        popupbuff  = makeBufferTiles BuffPopup
                       (length pubds) False (32,32)
        Dyns btbds = buff Map.! BuffPopup
        putextbuff = makeBufferTiles BuffPUText
                       (length btbds) False (1,1)
        Dyns mbds  = buff Map.! BuffMap
        mapbuff    = makeBufferTiles BuffMap
                       (length mbds) True (16,16)
        Dyns ldbds = buff Map.! BuffLoadScreen
        loadbuff   = makeBufferTiles BuffLoadScreen
                       (length ldbds) False (1,1)
        
--        linkbuff   = makeBufferTiles BuffLink       64   True (32,32)
--        buttbuff   = makeBufferTiles BuffButt       64   True (32,32)
--        textbuff   = makeBufferTiles BuffText       512  True (1,1)
--        popupbuff  = makeBufferTiles BuffPopup      64   True (32,32)
--        putextbuff = makeBufferTiles BuffPUText     256  True (1,1)
--        mapbuff    = makeBufferTiles BuffMap        32   True (16,16)
--        loadbuff   = makeBufferTiles BuffLoadScreen 32   True (1,1)

-- | this is an empty list n long for a texture b, what i use for buff
makeTileBuff ∷ BuffIndex → Int → [Tile]
makeTileBuff b n
  | n ≡ 0     = []
  | otherwise = makeTileBuff b (n - 1) ⧺ [tile]
  where tile = DTile (DMBuff b (n - 1)) (0,0) (1,1) (0,0) (1,1) False 0

-- | figure out what size the textbox should be
calcTextBoxSize ∷ String → [TTFData] → (Double,Double)
calcTextBoxSize str ttfdat
  = (max 1 (calcTBWidth str ttfdat)
    ,fromIntegral (length (splitOn ['\n'] str)))
calcTBWidth ∷ String → [TTFData] → Double
calcTBWidth []        _      = 0.1
calcTBWidth (' ':str) ttfdat = 0.1 + calcTBWidth str ttfdat
calcTBWidth (ch:str)  ttfdat = case indexTTFData ttfdat ch of
  Nothing → calcTBWidth str ttfdat
  Just d0 → chA + calcTBWidth str ttfdat
    where (TTFData _ _ (GlyphMetrics _ _ _ _ chA)) = d0
