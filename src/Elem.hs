-- | elems are abstract UI elements in each page
module Elem where
-- main elem functions are found
import Prelude()
import UPrelude
import Data ( Difficulty (..), Popup(..), PopupType(..) )
import Elem.Data ( WinElem(..), ButtAction(..)
                 , Button(..), ButtFunc(..), TextButton(..)
                 , InputAct(..), LuaFunc(..), CapType(..) )
import Load.Data ( DrawState(..), Tile(..), DSStatus(..), LoadCmd(..) )
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
    where ds'  = ds { dsWins   = changePageInWins (dsWins ds) ind win page
                    , dsWinsState = newWinsState
                    , dsStatus = DSSReload }
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
execButtonFunc ∷ (MonadLog μ, MonadFail μ)
  ⇒ DrawState → Int → String → String → LogT μ DrawState
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

-- | executes the load button action atttached to a button
execButtonLoad ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → Int → String → String → LogT μ DrawState
execButtonLoad ds ind win page = do
  log' LogInfo "blop"
  return ds

-- | executes the text button action atttached to a button
execButtonText ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → Int → String → String → LogT μ DrawState
execButtonText ds ind win page = do
    -- first we do any pure changes to the state
    let ds' = ds { dsWins   = findText (dsWins ds) ind win page
                 , dsPopup  = addNewPopup (dsPopup ds) (dsWins ds) ind win page
                 , dsStatus = DSSReload }
    case buttonChanges (dsWins ds') of
      Just ia → sendInpAct ia ≫ return ds'
      Nothing → return ds'

-- | checks for any non pure side effects that we need to change
buttonChanges ∷ [Window] → Maybe InputAct
buttonChanges []     = Nothing
buttonChanges (w:ws) = case pageButtonChanges (winPages w) of
  Nothing → buttonChanges ws
  Just ia → Just ia
pageButtonChanges ∷ [Page] → Maybe InputAct
pageButtonChanges []     = Nothing
pageButtonChanges (p:ps) = case elemButtonChanges (pageElems p) of
  Nothing → pageButtonChanges ps
  Just ia → Just ia
elemButtonChanges ∷ [WinElem] → Maybe InputAct
elemButtonChanges []       = Nothing
elemButtonChanges (we:wes) = case we of
  -- TODO: sort out why there are two of these
  WinElemButt _ _ _ _ (ButtActionText tb) _ _ _ → case buttonChange tb of
    Nothing → elemButtonChanges wes
    Just ia → Just ia
  WinElemButt _ _ _ _ (ButtActionKey 0 kf _) _ _ _ → Just $ InpActSetCap $ CapKeyChange 1 kf
  _                             → elemButtonChanges wes
buttonChange ∷ TextButton → Maybe InputAct
buttonChange (TextKeyMap (kf,_)) = Just $ InpActSetCap $ CapKeyChange 1 kf
buttonChange _ = Nothing

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
  WinElemButt _ _ _ _ (ButtActionKey _ k1 k2) i _ _ → if i ≡ ind then pu
    else elemPopup popup wes ind
    -- TODO: this only supports one or two popups at a time,
    --       create a popup tracking and sorting system
    where pu = if length popup > 0 then case head popup of
                 Popup pos size (PopupSetKey 1 _  _ ) → [Popup pos size $ PopupSetKey 2 k1 k2]
                 Popup _ _ (PopupSetKey 2 _  _ ) → []
                 _ → [Popup (0,0) (20,8) $ PopupSetKey 1 k1 k2]
               else [Popup (0,0) (20,8) $ PopupSetKey 1 k1 k2]
  _                             → elemPopup popup wes ind

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
