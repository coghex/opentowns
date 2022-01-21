-- | commands that alter the draw state are processed
module Load.Cmd where
-- load commands are seperate because they shouldnt be cluttering
-- up the main loading thread.
import Prelude()
import UPrelude
import Data ( Key(..), KeyFunc(..), LoadState(..) )
import Elem.Data ( WinElem(..), Button(..), ButtFunc(..), ButtAction(..) )
import Load.Data ( DrawState(..), DrawStateCmd(..)
                 , LoadCmd(..), DSStatus(..), WinsState(..) )
import Load.Popup ( findAndClearPopup, findAndUpdatePopup )
import Luau.Command ( unsanitizeKeyFunc, unsanitizeKeys )
import Luau.Data ( Window(..), Page(..) )
import Sign.Log ( MonadLog(..), LogT(..), sendLoadCmd, sendSettings )
import Sign.Data ( SettingsChange(..) )

-- | handles possible commands designed to change the draw state
processDrawStateCommand ∷ (MonadLog μ, MonadFail μ)
  ⇒ DrawState → DrawStateCmd → LogT μ DrawState
processDrawStateCommand ds (DSCToggleButts butts b) = do
  sendLoadCmd LoadCmdDyns
  if length butts ≡ 0 then
    return $ ds { dsWins = allButtsOff (dsWins ds) }
  else
    return $ ds { dsWins = toggleButts b butts (dsWins ds) }
processDrawStateCommand ds (DSCUpdatePopup pu) = return ds'
  where ds' = ds { dsPopup  = findAndUpdatePopup (dsPopup ds) pu
                 , dsStatus = DSSReload }
processDrawStateCommand ds (DSCClearPopup pu)  = return ds'
  where ds' = ds { dsPopup  = findAndClearPopup (dsPopup ds) pu
                 , dsStatus = DSSReload }
processDrawStateCommand ds (DSCUpdateKeyButton kf ks) = return ds'
  where ds' = ds { dsWins = updateKeyButton
                              (dsWins ds) (dsWinsState ds) kf ks }
processDrawStateCommand ds (DSCSavename str)  = do
  sendSettings $ SettingsChangeSavename str
  return ds'
  where ds' = ds { dsStatus = DSSLoadScreen }
processDrawStateCommand ds DSCLoadMap         = do
  return ds'
  where ds' = ds { dsWinsState = ws { loading = Loaded }
                 , dsStatus    = DSSRecreate }
        ws  = dsWinsState ds
processDrawStateCommand ds _                  = return ds

-- | updates the keys listed for a change key button
updateKeyButton ∷ [Window] → WinsState → KeyFunc → [Key] → [Window]
updateKeyButton []     _      _  _  = []
updateKeyButton (w:ws) winsSt kf ks
  | winTitle w ≡ current = [w'] ⧺ updateKeyButton ws winsSt kf ks
  | otherwise            = [w]  ⧺ updateKeyButton ws winsSt kf ks
    where w'      = w { winPages
                     = updateKeyButtonInWin (winPages w) kf ks }
          current = thisWin winsSt
updateKeyButtonInWin ∷ [Page] → KeyFunc → [Key] → [Page]
updateKeyButtonInWin []     _  _  = []
updateKeyButtonInWin (p:ps) kf ks = [p'] ⧺ updateKeyButtonInWin ps kf ks
  where p' = p { pageElems = updateKeyButtonInPage (pageElems p) kf ks }
updateKeyButtonInPage ∷ [WinElem] → KeyFunc → [Key] → [WinElem]
updateKeyButtonInPage []       _  _  = []
updateKeyButtonInPage (we:wes) kf ks = [we']
  ⧺ updateKeyButtonInPage wes kf ks
  where we' = updateKeyButtonInElem we kf ks
updateKeyButtonInElem ∷ WinElem → KeyFunc → [Key] → WinElem
updateKeyButtonInElem
  (WinElemButt a b c d (ButtActionKey n0 kf0 ks0) e f g) kf ks
    = if kf0 ≡ kf then WinElemButt a b c d (ButtActionKey 0 kf ks) e s g
      else WinElemButt a b c d (ButtActionKey n0 kf0 ks0) e f g
        where s = unsanitizeKeyFunc kf ⧺ unsanitizeKeys ks
updateKeyButtonInElem we _  _  = we

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
toggleWinElemButt b (Button (ButtFuncFunc i) _ _ _ _)
  (WinElemButt pos col box adv act ind val _)
    | i ≡ ind   = WinElemButt pos col box adv act ind val b
    | otherwise = WinElemButt pos col box adv act ind val False
toggleWinElemButt b (Button (ButtFuncText i) _ _ _ _)
  (WinElemButt pos col box adv act ind val _)
    | i ≡ ind   = WinElemButt pos col box adv act ind val b
    | otherwise = WinElemButt pos col box adv act ind val False
toggleWinElemButt b (Button (ButtFuncLoad i) _ _ _ _)
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
