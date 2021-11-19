-- | elems are abstract UI elements in each page
module Elem where
-- main elem functions are found
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..), ButtAction(..)
                 , Button(..), ButtFunc(..)
                 , InputAct(..) )
import Load.Data ( Tile(..) )
import Luau.Data ( Page(..), Window(..) )
import Prog.Data ( Env(..) )
import Sign.Log ( LogT(..), MonadLog(..), sendInpAct )
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

-- | finds the current page, if we cant find it just use the first page
currentPage ∷ Window → Page
currentPage (Window _ _ pages curr _)
  = findCurrentPage (head pages) pages curr
findCurrentPage ∷ Page → [Page] → String → Page
findCurrentPage p0 []     _    = p0
findCurrentPage p0 (p:ps) curr
  | pageTitle p ≡ curr = p
  | otherwise          = findCurrentPage p0 ps curr

-- | sets up element input mapping
initElem ∷ (MonadLog μ, MonadFail μ)
  ⇒ String → String → WinElem → Int → LogT μ WinElem
initElem win page
  (WinElemButt pos col box adv (ButtActionLink dest) _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncLink ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  --atomically $ writeQueue (envInpQ env) $ InpActSetLink butt
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv (ButtActionLink dest) ind args hov
initElem _   _   we       _ = return we     
