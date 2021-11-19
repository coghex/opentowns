-- | commands that alter the draw state are processed
module Load.Cmd where
-- load commands are seperate because they shouldnt be cluttering
-- up the main loading thread.
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..) )
import Load.Data ( DrawState(..), DrawStateCmd(..) )
import Luau.Data ( Window(..), Page(..) )

-- TODO: this should only toggle listed buttons, not all
processDrawStateCommand ∷ DrawState → DrawStateCmd → IO DrawState
processDrawStateCommand ds (DSCToggleButts _) = return
  $ ds { dsWins = toggleButts (dsWins ds) }
processDrawStateCommand ds _                  = return ds

toggleButts ∷ [Window] → [Window]
toggleButts []     = []
toggleButts (w:ws) = [w'] ⧺ toggleButts ws
  where w' = w { winPages = togglePageButts (winPages w) }

togglePageButts ∷ [Page] → [Page]
togglePageButts []     = []
togglePageButts (p:ps) = [p'] ⧺ togglePageButts ps
  where p' = p { pageElems = togglePageElemButts (pageElems p) }

togglePageElemButts ∷ [WinElem] → [WinElem]
togglePageElemButts []       = []
togglePageElemButts (we:wes) = [we'] ⧺ togglePageElemButts wes
  where we' = toggleWinElemButt we

toggleWinElemButt ∷ WinElem → WinElem
toggleWinElemButt (WinElemButt pos col box adv act ind val h)
  = WinElemButt pos col box adv act ind val (not h)
toggleWinElemButt we = we
