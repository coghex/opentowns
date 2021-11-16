-- | windows are abstract representations of glfw windows,
--   filled with abstract 'pages' which hold 'elems'.
--   pages are like different screens, elems are like
--   individual text lines or buttons...
module Luau.Window where
-- functions to help manipulate windows
import Prelude()
import UPrelude
import Luau.Data ( Window(..), Page(..) )


-- | returns maybe the head window
currentWin ∷ [Window] → Maybe Window
currentWin wins
  | length wins ≤ 0 = Nothing
  | otherwise       = Just $ head wins

-- | switchs to the specified window
switchWin ∷ String → [Window] → [Window]
switchWin name wins = [win1] ⧺ olds
  where olds = filter (\w → winTitle w ≠ name) wins
        win0 = head $ filter (\w → winTitle w ≠ name) wins
        win1 = win0 { winLast = winTitle $ head olds }

-- | adds a page to a window
addPageToWin ∷ String → Page → [Window] → [Window]
addPageToWin _    _    []     = []
addPageToWin name page (w:ws)
  | winTitle w ≡ name = [w'] ⧺ addPageToWin name page ws
  | otherwise         = [w]  ⧺ addPageToWin name page ws
      where w' = w { winPages = winPages w ⧺ [page] }
