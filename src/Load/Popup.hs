-- | popups are seperate from windowing, and persist over them
module Load.Popup where
-- some helper commands for keeping track of popups
import Prelude()
import UPrelude
import Data ( Popup(..), PopupType(..) )


-- | checks to make sure we dont already have a popup for set key
addToPopups ∷ [Popup] → PopupType → [Popup]
addToPopups []             pu = [Popup (0,0) (20,8) pu]
addToPopups (popup:popups) pu
  | puType popup ≡ pu = [popup] ⧺ popups 
  | otherwise         = [popup] ⧺ addToPopups popups pu

-- | clears first popup of designated type
findAndClearPopup ∷ [Popup] → PopupType → [Popup]
findAndClearPopup []             _  = []
findAndClearPopup (popup:popups) pu = if comparePT (puType popup) pu
  then popups
  else [popup] ⧺ findAndClearPopup popups pu
comparePT ∷ PopupType → PopupType → Bool
comparePT PopupSetKey {}   PopupSetKey {}   = True
comparePT PopupSavename {} PopupSavename {} = True
comparePT _                _                = False

-- | updates first popup of type with new data
findAndUpdatePopup ∷ [Popup] → PopupType → [Popup]
findAndUpdatePopup []             _  = []
findAndUpdatePopup (popup:popups) pu = if comparePT (puType popup) pu
  then [popup'] ⧺ popups
  else [popup] ⧺ findAndUpdatePopup popups pu
  where popup' = updatePopup popup pu
updatePopup ∷ Popup → PopupType → Popup
updatePopup popup pu = popup { puType = pu }

-- | a popup to ask for the name of a save
saveGamePopup ∷ Popup
saveGamePopup = Popup (0,0) (20,8) $ PopupSavename []
