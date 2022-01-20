-- | elems are abstract UI elements in each page
module Elem where
-- main elem functions are found
import Prelude()
import UPrelude
import Data.List.Split (splitOn)
import Data ( Difficulty (..), Popup(..), PopupType(..) )
import Elem.Data ( WinElem(..), ButtAction(..)
                 , Button(..), ButtFunc(..), TextButton(..)
                 , InputAct(..), LuaFunc(..), CapType(..) )
import Elem.World ( genMapTiles )
import Load.Data ( DrawState(..), Tile(..), DSStatus(..), LoadCmd(..) )
import Load.Popup ( addToPopups, saveGamePopup )
import Luau.Data ( Page(..), Window(..) )
import Luau.Window ( currentWin )
import Sign.Data ( LogLevel(..), SysAction(..) )
import Sign.Log ( LogT(..), MonadLog(..), sendInpAct, log', sendSys, toggleFullScreen )
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

-- | sets up element input mapping for buttons, calls world generation
--   for map elem
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
initElem win page
  (WinElemButt pos col box adv (ButtActionText text) _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncText ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv (ButtActionText text) ind args hov
initElem win page
  (WinElemButt pos col box adv (ButtActionLoad dest argv) _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncLoad ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv (ButtActionLoad dest argv) ind args hov
initElem win page
  (WinElemButt pos col box adv (ButtActionKey n k1 k2) _ args hov) ind = do
  let butt = Button { bFunc = ButtFuncText ind
                    , bPos  = pos
                    , bSize = box
                    , bWin  = win
                    , bPage = page }
  sendInpAct $ InpActSetLink butt
  return $ WinElemButt pos col box adv (ButtActionKey n k1 k2) ind args hov
initElem win page (WinElemMap maptype maptiles) _ = return newMap
  where newMap = WinElemMap maptype $ genMapTiles maptype
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
    log' (LogDebug 1) new
    return ds'
    where ds'  = ds { dsWins
                        = changePageInWins (dsWins ds) ind win page
                    , dsWinsState = newWinsState
                    , dsStatus    = DSSReload }
          newWinsState = changePageInWinsState (dsWinsState ds) (dsWins ds) ind win page
processButton ds (Button (ButtFuncFunc ind) _ _ win page)
  = execButtonFunc ds ind win page
processButton ds (Button (ButtFuncLoad ind) _ _ win page)
  = execButtonLoad ds ind win page
processButton ds (Button (ButtFuncText ind) _ _ win page)
  = execButtonText ds ind win page
processButton ds _ = return ds


-- | we go though all that work again just to find the names
changePageInWinsState ∷ ((String,String),(String,String))
  → [Window] → Int → String → String
  → ((String,String),(String,String))
changePageInWinsState oldwinsstate []     _    _   _    = oldwinsstate
changePageInWinsState oldwinsstate (w:ws) dest win page
 | winTitle w ≡ win = ((cwin,fst (fst oldwinsstate))
                      ,(cpage,fst (snd oldwinsstate)))
 | otherwise        = changePageInWinsState oldwinsstate ws dest win page
   where cpage
           = findButtonDestByInd dest page (winLast w) (winPages w)
        -- since we are only changing pages, we can assume the same win
         cwin = winTitle w
-- | changes the current page in the list of windows
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
-- | finds page referenced by button ind
findNewPageInWins ∷ [Window] → Int → String → String → String
findNewPageInWins [] _ _ _ = []
findNewPageInWins (w:ws) dest win page
  | winTitle w ≡ win = findButtonDestByInd dest page (winLast w) (winPages w)
  | otherwise        = findNewPageInWins ws dest win page

-- | finds win and page referenced by button ind
findNewWin ∷ [Window] → Int → String → (String,String)
findNewWin [] _ _ = ([],[])
findNewWin (w:ws) dest win
  | winTitle w ≡ win = (head list, last list)
  | otherwise        = findNewWin ws dest win
      where list = splitOn ":" $ findButtonDestByInd dest (winCurr w) (winLast w) (winPages w)

-- this only works if names are unique, returns win or page names
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
findButtonDestByIndElem n lt ((WinElemButt _ _ _ _ (ButtActionLoad newwin newpage) ind _ _):wes)
  | ind ≡ n   = newwin ⧺ ":" ⧺ newpage
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
execButtonFunc ∷ (MonadLog μ, MonadFail μ)
  ⇒ DrawState → Int → String → String → LogT μ DrawState
execButtonFunc ds ind win page = do
  let func = findFunc wins ind win page
      wins = dsWins ds
  case func of
    LuaFuncToggleFullScreen → do
      oldSize ← toggleFullScreen (dsOldSize ds)
      return ds { dsOldSize = oldSize }
    LuaFuncNewGame _ → do
      case buttonChanges ind (dsWins ds') of
        Just ia → sendInpAct ia ≫ return ds'
        Nothing → return ds'
      where ds' = ds { dsPopup  = dsPopup ds ⧺ [saveGamePopup]
                     , dsStatus = DSSReload }
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

-- | executes the load button action atttached to a button
execButtonLoad ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → Int → String → String → LogT μ DrawState
execButtonLoad ds ind win _ = do
    sendInpAct $ InpActSetPage new p
    log' (LogDebug 1) new
    return ds'
    where ds'     = ds { dsWinsState = newWinsState
                       , dsStatus    = DSSReload }
          (new,p) = findNewWin (dsWins ds) ind win
          newWinsState = ((new,lwin),(p,lpage))
          ((lwin,_),(lpage,_)) = dsWinsState ds

-- | executes the text button action atttached to a button
execButtonText ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → Int → String → String → LogT μ DrawState
execButtonText ds ind win page = do
    -- first we do any pure changes to the state
    let ds' = ds { dsWins   = findText (dsWins ds) ind win page
                 , dsPopup  = addNewPopup (dsPopup ds) (dsWins ds)
                                          ind win page
                 , dsStatus = DSSReload }
    case buttonChanges ind (dsWins ds') of
      Just ia → sendInpAct ia ≫ return ds'
      Nothing → return ds'

-- | checks for any non pure side effects that we need to change
buttonChanges ∷ Int → [Window] → Maybe InputAct
buttonChanges _   []     = Nothing
buttonChanges ind (w:ws) = case pageButtonChanges ind (winPages w) of
  Nothing → buttonChanges ind ws
  Just ia → Just ia
pageButtonChanges ∷ Int → [Page] → Maybe InputAct
pageButtonChanges _   []     = Nothing
pageButtonChanges ind (p:ps) = case elemButtonChanges ind (pageElems p) of
  Nothing → pageButtonChanges ind ps
  Just ia → Just ia
elemButtonChanges ∷ Int → [WinElem] → Maybe InputAct
elemButtonChanges _   []       = Nothing
elemButtonChanges ind (we:wes) = case we of
  WinElemButt _ _ _ _ (ButtActionKey _ kf _) i _ _ → if i ≡ ind then
    Just $ InpActSetCap $ CapKeyChange 1 kf
    else elemButtonChanges ind wes
  WinElemButt _ _ _ _ (ButtActionFunc (LuaFuncNewGame _)) i _ _
    → if i ≡ ind then
      Just $ InpActSetCap $ CapTextInput []
      else elemButtonChanges ind wes
  _                             → elemButtonChanges ind wes

findText ∷ [Window] → Int → String → String → [Window]
findText []      _   _   _    = []
findText (w:wes) ind win page
  | winTitle w ≡ win = [w'] ⧺ findText wes ind win page
  | otherwise        = [w]  ⧺ findText wes ind win page
      where w' = w { winPages = findTextInPage (winPages w) ind page }

findTextInPage ∷ [Page] → Int → String → [Page]
findTextInPage []     _   _    = []
findTextInPage (p:ps) ind page
  | pageTitle p ≡ page = [p'] ⧺ findTextInPage ps ind page
  | otherwise          = [p]  ⧺ findTextInPage ps ind page
      where p' = p { pageElems = findTextInElems (pageElems p) ind }

findTextInElems ∷ [WinElem] → Int → [WinElem]
findTextInElems []       _   = []
findTextInElems (we:wes) ind = [we'] ⧺ findTextInElems wes ind
  where we' = case we of
                WinElemButt a b c d act i e f → if i ≡ ind then
                    WinElemButt a b c d (evalTextButtAction act) i e f
                  else we
                _                             → we

-- | new popups can be created by text buttons, some popup types
--   check if an existing popup of the same type exists already
addNewPopup ∷ [Popup] → [Window] → Int → String → String → [Popup]
addNewPopup pus []      _   _   _    = pus
addNewPopup pus (w:wes) ind win page
  | winTitle w ≡ win = winPopup pus (winPages w) ind page
  | otherwise        = addNewPopup pus wes ind win page

winPopup ∷ [Popup] → [Page] → Int → String → [Popup]
winPopup _     []     _   _  = []
winPopup popup (p:ps) ind page
  | pageTitle p ≡ page = elemPopup popup (pageElems p) ind
  | otherwise          = winPopup popup ps ind page

elemPopup ∷ [Popup] → [WinElem] → Int → [Popup]
elemPopup popup []       _   = popup
elemPopup popup (we:wes) ind = case we of
  WinElemButt _ _ _ _ (ButtActionKey a b c) i _ _ → if i ≡ ind then pu
    else elemPopup popup wes ind
    where pu = addToPopups popup $ PopupSetKey 1 b [head c]
  _ → elemPopup popup wes ind

--    where pu = if length popup > 0 then case head popup of
--                 Popup _ _ (PopupSetKey 1 _  _ ) → [Popup (0,0) (10,8) $ PopupSetKey 2 k1 k2]
--                 Popup _ _ (PopupSetKey 2 _  _ ) → []
--                 _ → [Popup (0,0) (20,8) $ PopupSetKey 1 k1 k2]
--               else [Popup (0,0) (20,8) $ PopupSetKey 1 k1 k2]
--  _                             → elemPopup popup wes ind

-- | only need to update the values of text buttons atm
evalTextButtAction ∷ ButtAction → ButtAction
evalTextButtAction (ButtActionText tb)
  = ButtActionText $ findTextElem tb
evalTextButtAction (ButtActionKey n k1 k2)
  = ButtActionKey (n+1) k1 k2
evalTextButtAction ba = ba
-- | enumeration of what happens when a textButton is pressed
findTextElem ∷ TextButton → TextButton
findTextElem (TextMusic          b) = TextMusic          $ not b
findTextElem (TextMusicVolume    v) = TextMusicVolume    $ inc100 v
findTextElem (TextFX             b) = TextFX             $ not b
findTextElem (TextFXVolume       v) = TextFXVolume       $ inc100 v
findTextElem (TextMouseScroll    b) = TextMouseScroll    $ not b
findTextElem (TextScrollHover    b) = TextScrollHover    $ not b 
findTextElem (TextHeightCubes    b) = TextHeightCubes    $ not b
findTextElem (TextItemDisableDef b) = TextItemDisableDef $ not b
findTextElem (TextPauseOnStart   b) = TextPauseOnStart   $ not b
findTextElem (TextAutosave       v) = TextAutosave       $ incAutoSave v
findTextElem (TextSieges         v) = TextSieges         $ incDifficulty v
findTextElem (TextPauseOnSiege   b) = TextPauseOnSiege   $ not b
findTextElem (TextPauseOnCaravan b) = TextPauseOnCaravan $ not b
findTextElem (TextAllowBury      b) = TextAllowBury      $ not b
findTextElem (TextCPULevel       v) = TextCPULevel       $ incCPU v
findTextElem (TextKeyMap         v) = TextKeyMap           v
findTextElem tb                     = tb
-- | increments a 0-100% scale in 10's
inc100 ∷ Int → Int
inc100 100 = 0
inc100 n   = n + 10
-- | increments maybe 1-9
incAutoSave ∷ Maybe Int → Maybe Int
incAutoSave (Just 10) = Nothing
incAutoSave Nothing   = Just 1
incAutoSave (Just n)  = Just (n + 1)
-- | increments difficulty values
incDifficulty ∷ Difficulty → Difficulty
incDifficulty DNormal   = DHard
incDifficulty DHard     = DHarder
incDifficulty DHarder   = DInsane
incDifficulty DInsane   = DDisabled
incDifficulty DDisabled = DEasy
incDifficulty DEasy     = DNormal
incDifficulty _         = DNULL
-- | increments 1-6
incCPU ∷ Int → Int
incCPU 1 = 2
incCPU 2 = 3
incCPU 3 = 4
incCPU 4 = 5
incCPU 5 = 6
incCPU 6 = 1
incCPU _ = -1
