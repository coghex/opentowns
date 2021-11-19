-- | elems are abstract UI elements in each page
module Elem where
-- main elem functions are found
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..), ButtAction(..), Button(..), ButtFunc(..) )
import Load.Data ( Tile(..) )
import Luau.Data ( Page, Window(winSize, winPages) )
import Prog.Data ( Env(..), InputAct(..) )
import Sign.Var ( atomically )
import Sign.Queue ( writeQueue )
import Vulk.Font ( TTFData(..) )

-- | finds tiles from a window
loadWindow ∷ Int → Window → [TTFData] → [Tile]
loadWindow nDefTex win ttfdat
  = loadPageElems ttfdat nDefTex (winSize win) (winPages win)

-- | finds tiles from a list of pages
loadPageElems ∷ [TTFData] → Int → (Int,Int) → [Page] → [Tile]
loadPageElems _      _       _    []     = []
loadPageElems ttfdat nDefTex size (p:ps)
  = loadPageElem ttfdat nDefTex size p ⧺ loadPageElems ttfdat nDefTex size ps
loadPageElem ∷ [TTFData] → Int → (Int,Int) → Page → [Tile]
loadPageElem _      _ _    _    = [] -- calcText ttfdat color (fst pos') pos' str
  -- we use old openGL convention since towns justifies everything top left
--  where pos'     = (0,0)-- ((fst pos) - ((fst sizeNorm)),(-(snd pos)) + ((snd sizeNorm)))
--        sizeNorm = (1,1)-- ((fromIntegral (fst size))/128.0,(fromIntegral (snd size))/128.0)

-- | sets up element input mapping
initElem ∷ Env → String → WinElem → Int → IO WinElem
initElem env win (WinElemButt pos col box adv (ButtActionLink dest) _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncLink ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win }
  atomically $ writeQueue (envInpQ env) $ InpActSetLink butt
  return $ WinElemButt pos col box adv (ButtActionLink dest) ind args hov
initElem _  _   we       _ = return we     
