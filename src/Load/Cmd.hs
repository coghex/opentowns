-- | commands that alter the draw state are processed
module Load.Cmd where
-- load commands are seperate because they shouldnt be cluttering
-- up the main loading thread.
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..) )
import Load.Data ( DrawState(..), DrawStateCmd(..)
                 , LoadCmd(..), DSStatus(..) )
import Luau.Data ( Window(..), Page(..) )
import Sign.Log ( MonadLog(..), LogT(..), sendLoadCmd )

-- TODO: this should only toggle listed buttons, not all
processDrawStateCommand ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → DrawStateCmd → LogT μ DrawState
processDrawStateCommand ds (DSCToggleButts _ b) = do
  sendLoadCmd LoadCmdDyns
  return $ ds { dsWins   = toggleButts b (dsWins ds) }
processDrawStateCommand ds _                  = return ds

toggleButts ∷ Bool → [Window] → [Window]
toggleButts _ []     = []
toggleButts b (w:ws) = [w'] ⧺ toggleButts b ws
  where w' = w { winPages = togglePageButts b (winPages w) }

togglePageButts ∷ Bool → [Page] → [Page]
togglePageButts _ []     = []
togglePageButts b (p:ps) = [p'] ⧺ togglePageButts b ps
  where p' = p { pageElems = togglePageElemButts b (pageElems p) }

togglePageElemButts ∷ Bool → [WinElem] → [WinElem]
togglePageElemButts _ []       = []
togglePageElemButts b (we:wes) = [we'] ⧺ togglePageElemButts b wes
  where we' = toggleWinElemButt b we

toggleWinElemButt ∷ Bool → WinElem → WinElem
toggleWinElemButt b (WinElemButt pos col box adv act ind val _)
  = WinElemButt pos col box adv act ind val b
toggleWinElemButt _ we = we
