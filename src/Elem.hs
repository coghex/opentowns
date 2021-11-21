-- | elems are abstract UI elements in each page
module Elem where
-- main elem functions are found
import Prelude()
import UPrelude
import Data.List.Split ( splitOn )
import Elem.Data ( WinElem(..), ButtAction(..)
                 , Button(..), ButtFunc(..)
                 , InputAct(..), LuaFunc(..) )
import Load.Data ( DrawState(..), Tile(..), DSStatus(..) )
import Luau.Data ( Page(..), Window(..) )
import Prog.Data ( Env(..) )
import Sign.Data ( LogLevel(..), SysAction(..) )
import Sign.Log ( LogT(..), MonadLog(..), sendInpAct, log', sendSys, toggleFullScreen )
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
initElem win page
  (WinElemButt pos col box adv ButtActionBack _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncLink ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv ButtActionBack ind args hov
initElem win page
  (WinElemButt pos col box adv ButtActionExit _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncLink ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv ButtActionExit ind args hov
initElem win page
  (WinElemButt pos col box adv (ButtActionFunc func) _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncFunc ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv (ButtActionFunc func) ind args hov
initElem _   _   we       _ = return we

-- | handles individual button presses
processButton ∷ (MonadLog μ, MonadFail μ)
  ⇒ DrawState → Button → LogT μ DrawState
processButton ds (Button (ButtFuncLink ind) _ _ win page) = do
  -- there are some reserved menu names, for simplicity
  let new  = findNewPageInWins (dsWins ds) ind win page
  if new ≡ "EXIT" then sendSys SysExit ≫ return ds
  else do
    sendInpAct $ InpActSetPage win new
    log' LogInfo new
    return ds'
    where ds'  = ds { dsWins   = changePageInWins (dsWins ds) ind win page
                    , dsStatus = DSSReload }
processButton ds (Button (ButtFuncFunc ind) _ _ win page)
  = execButtonFunc ds ind win page
processButton ds _ = return ds
changePageInWins ∷ [Window] → Int → String → String → [Window]
changePageInWins []     _    _   _    = []
changePageInWins (w:ws) dest win page
  | winTitle w ≡ win = [w'] ⧺ changePageInWins ws dest win page
  | otherwise        = [w]  ⧺ changePageInWins ws dest win page
    where w'  = changePage w new
          new = findButtonDestByInd dest page (winLast w) (winPages w)
changePage ∷ Window → String → Window
changePage win page = win { 
                            winCurr = page
                          , winLast = winCurr win }
findNewPageInWins ∷ [Window] → Int → String → String → String
findNewPageInWins [] _ _ _ = []
findNewPageInWins (w:ws) dest win page
  | winTitle w ≡ win = findButtonDestByInd dest page (winLast w) (winPages w)
  | otherwise        = findNewPageInWins ws dest win page

-- this only works if names are unique
findButtonDestByInd ∷ Int → String → String → [Page] → String
findButtonDestByInd _ _    _    []     = []
findButtonDestByInd n page lt (p:ps)
  | page ≡ pageTitle p = findButtonDestByIndElem n lt (pageElems p)
  | otherwise          = findButtonDestByInd n page lt ps
findButtonDestByIndElem ∷ Int → String → [WinElem] → String
findButtonDestByIndElem _ _    []       = []
findButtonDestByIndElem n lt ((WinElemButt _ _ _ _ (ButtActionLink dest) ind _ _):wes)
  | ind ≡ n   = dest
  | otherwise = findButtonDestByIndElem n lt wes
findButtonDestByIndElem n lt ((WinElemButt _ _ _ _ ButtActionBack ind _ _):wes)
  | ind ≡ n   = lt
  | otherwise = findButtonDestByIndElem n lt wes
findButtonDestByIndElem n lt ((WinElemButt _ _ _ _ ButtActionExit ind _ _):wes)
  | ind ≡ n   = "EXIT"
  | otherwise = findButtonDestByIndElem n lt wes
findButtonDestByIndElem n lt (_:wes) = findButtonDestByIndElem n lt wes

-- | returns the amount of page elements currently in all pages
--   the reason it looks so nifty is that it was written by hlint
lengthAllElems ∷ [Window] → Int
lengthAllElems = foldr ((+) . lengthPageElems . winPages) 0
lengthPageElems ∷ [Page] → Int
lengthPageElems = foldr ((+) . length . pageElems) 0

-- | executes the function atttached to a button
execButtonFunc ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → Int → String → String → LogT μ DrawState
execButtonFunc ds ind win page = do
  let func = findFunc wins ind win page
      wins = dsWins ds
  case func of
    LuaFuncToggleFullScreen → do
      oldSize ← toggleFullScreen (dsOldSize ds)
      return ds { dsOldSize = oldSize }
    LuaFuncUnknown str → do
      log' LogWarn $ "unknown lua command: " ⧺ str
      return ds
    LuaFuncNULL → log' LogError "lua NULL function" ≫ return ds

findFunc ∷ [Window] → Int → String → String → LuaFunc
findFunc []      _   _   _    = LuaFuncNULL
findFunc (w:wes) ind win page
  | winTitle w ≡ win = findFuncInPage (winPages w) ind page
  | otherwise        = findFunc wes ind win page

findFuncInPage ∷ [Page] → Int → String → LuaFunc
findFuncInPage []     _   _    = LuaFuncNULL
findFuncInPage (p:ps) ind page
  | pageTitle p ≡ page = findFuncInElems (pageElems p) ind
  | otherwise          = findFuncInPage ps ind page

findFuncInElems ∷ [WinElem] → Int → LuaFunc
findFuncInElems []       _   = LuaFuncNULL
findFuncInElems (we:wes) ind = case we of
  WinElemButt _ _ _ _ (ButtActionFunc func) i _ _ → if i ≡ ind then func
    else findFuncInElems wes ind
  _ → findFuncInElems wes ind
