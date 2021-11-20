-- | commands that alter the draw state are processed
module Load.Cmd where
-- load commands are seperate because they shouldnt be cluttering
-- up the main loading thread.
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..), Button(..), ButtFunc(..) )
import Load.Data ( DrawState(..), DrawStateCmd(..)
                 , LoadCmd(..), DSStatus(..) )
import Luau.Data ( Window(..), Page(..) )
import Sign.Log ( MonadLog(..), LogT(..), sendLoadCmd )

-- | handles possible commands designed to change the draw state
processDrawStateCommand ∷ (MonadLog μ, MonadFail μ)
  ⇒ DrawState → DrawStateCmd → LogT μ DrawState
processDrawStateCommand ds (DSCToggleButts butts b) = do
  sendLoadCmd LoadCmdDyns
  if length butts ≡ 0 then
    return $ ds { dsWins = allButtsOff (dsWins ds) }
  else
    return $ ds { dsWins = toggleButts b butts (dsWins ds) }
processDrawStateCommand ds _                  = return ds

-- | toggles the desired buttons in the list of windows
toggleButts ∷ Bool → [Button] → [Window] → [Window]
toggleButts _ _    []     = []
toggleButts b butt (w:ws) = [w'] ⧺ toggleButts b butt ws
  where w' = w { winPages = togglePageButts b butt (winPages w) }

togglePageButts ∷ Bool → [Button] → [Page] → [Page]
togglePageButts _ _    []     = []
togglePageButts b butt (p:ps) = [p'] ⧺ togglePageButts b butt ps
  where p' = p { pageElems = togglePageElemButts b butt (pageElems p) }

togglePageElemButts ∷ Bool → [Button] → [WinElem] → [WinElem]
togglePageElemButts _ _    []       = []
togglePageElemButts b butt (we:wes)
  = [we'] ⧺ togglePageElemButts b butt wes
    where we' = toggleWinElemButts b butt we

toggleWinElemButts ∷ Bool → [Button] → WinElem → WinElem
toggleWinElemButts _ []     we = we
toggleWinElemButts bool (b:bs) we = toggleWinElemButts bool bs we'
  where we' = toggleWinElemButt bool b we
toggleWinElemButt ∷ Bool → Button → WinElem → WinElem
toggleWinElemButt b (Button (ButtFuncLink i) _ _ _ _)
  (WinElemButt pos col box adv act ind val _)
    | i ≡ ind   = WinElemButt pos col box adv act ind val b
    | otherwise = WinElemButt pos col box adv act ind val False
toggleWinElemButt _ _    we = we

-- | sets all buttons off when there are no butts under the mouse
allButtsOff ∷ [Window] → [Window]
allButtsOff []     = []
allButtsOff (w:ws) = [w'] ⧺ allButtsOff ws
  where w' = w { winPages = offPageButts (winPages w) }

offPageButts ∷ [Page] → [Page]
offPageButts []     = []
offPageButts (p:ps) = [p'] ⧺ offPageButts ps
  where p' = p { pageElems = offPageElemButts (pageElems p) }

offPageElemButts ∷ [WinElem] → [WinElem]
offPageElemButts []       = []
offPageElemButts (we:wes)
  = [we'] ⧺ offPageElemButts wes
    where we' = offWinElemButt we

offWinElemButt ∷ WinElem → WinElem
offWinElemButt (WinElemButt pos col box adv act ind val _)
  = WinElemButt pos col box adv act ind val False
offWinElemButt we = we
