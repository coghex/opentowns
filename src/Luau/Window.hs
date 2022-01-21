-- | windows are abstract representations of glfw windows,
--   filled with abstract 'pages' which hold 'elems'.
--   pages are like different screens, elems are like
--   individual text lines or buttons...
module Luau.Window where
-- functions to help manipulate windows
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..) )
import Load.Data ( WinsState(..) )
import Luau.Data ( Window(..), Page(..) )


-- | returns maybe the head window
currentWin ∷ [Window] → WinsState → Maybe Window
currentWin wins ws
  | length wins ≤ 0 = Nothing
  | otherwise       = findWin (thisWin ws) wins

-- | search wins by name
findWin ∷ String → [Window] → Maybe Window
findWin _    [] = Nothing
findWin name (win:wins)
  | winTitle win ≡ name = Just win
  | otherwise = findWin name wins

-- | switchs to the specified window
switchWin ∷ String → [Window] → [Window]
switchWin name wins = [win1] ⧺ olds
  where olds = filter (\w → winTitle w ≠ name) wins
        win0 = head $ filter (\w → winTitle w ≠ name) wins
        win1 = win0 { winCurr = name
                    , winLast = winTitle $ head olds }

-- | adds a page to a window
addPageToWin ∷ String → Page → [Window] → [Window]
addPageToWin _    _    []     = []
addPageToWin name page (w:ws)
  | winTitle w ≡ name = [w'] ⧺ addPageToWin name page ws
  | otherwise         = [w]  ⧺ addPageToWin name page ws
      where w' = w { winPages = winPages w ⧺ [page] }

-- | adds an elem into a page in a window
addElemToPageInWin ∷ String → String → WinElem → [Window] → [Window]
addElemToPageInWin _   _    _    []     = []
addElemToPageInWin win page el (w:ws)
  | win ≡ winTitle w = [w'] ⧺ addElemToPageInWin win page el ws
  | otherwise        = [w]  ⧺ addElemToPageInWin win page el ws
    where w'    = w { winPages = pages
    -- the first page added will become the current page
                    , winCurr  = if winCurr w ≡ "NULL" then page
                                 else winCurr w }
          pages = addElemToPage page el (winPages w)

-- | adds an elem into a page
addElemToPage ∷ String → WinElem → [Page] → [Page]
addElemToPage _    _  []     = []
addElemToPage name el (p:ps)
  | name ≡ pageTitle p = [p'] ⧺ addElemToPage name el ps
  | otherwise          = [p]  ⧺ addElemToPage name el ps
    where p'    = p { pageElems = pageElems p ⧺ [el] }

-- | sets the size of all windows when GLFW is resized
resizeWins ∷ (Int,Int) → [Window] → [Window]
resizeWins _    []         = []
resizeWins size (win:wins) = [win'] ⧺ resizeWins size wins
  where win' = win { winSize = size }

