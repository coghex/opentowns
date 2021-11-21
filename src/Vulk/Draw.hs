-- | draw is the middle layer translation between an abstract idea of
--   a hypothetical window, to a giant list of 2D tiles.  these still
--   need to be converted into verticies/indicies in Vulk.Calc
module Vulk.Draw where
-- convert epiklesis state to draw state
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Data ( Color(..) )
import Elem ( loadWindow )
import Load.Data ( DrawState(..), Tile(..), DynMap(..) )
import Prog.Buff ( makeBufferTiles )
import Vulk.Font
    ( indexTTFData, GlyphMetrics(GlyphMetrics), TTFData(TTFData) )
import Luau.Window (currentWin)

-- | loads tiles from drawstate
loadTiles ∷ DrawState → (Double,Double) → [TTFData] → [Tile]
loadTiles ds winSize ttfdat
  = [GTile (0,0) (w,h) (0,0) (1,1) 105 (Color 255 255 255 255)
  ,  GTile (0,0) (8,3) (0,0) (1,1) 106 (Color 255 255 255 255)]
  ⧺ winTiles
  where winTiles   = case currentWin (dsWins ds) of
          Just win → linkbuff ⧺ buttbuff ⧺ popupbuff ⧺ putextbuff
                     ⧺ textbuff ⧺ loadWindow nDefTex win ttfdat
          Nothing  → []
        nDefTex    = 0 -- dsNDefTex ds
        (w,h)      = winSize
        linkbuff   = makeBufferTiles 0 64   True (32,32)
        buttbuff   = makeBufferTiles 1 64   True (32,32)
        textbuff   = makeBufferTiles 2 512  True (1,1)
        popupbuff  = makeBufferTiles 3 64   True (32,32)
        putextbuff = makeBufferTiles 4 256  True (1,1)

-- | this is an empty list n long for a texture b, what i use for buff
makeTileBuff ∷ Int → Int → [Tile]
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
