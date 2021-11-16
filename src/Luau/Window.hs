-- | windows are abstract representations of glfw windows,
--   filled with abstract 'pages' which hold 'elems'.
--   pages are like different screens, elems are like
--   individual text lines or buttons...
module Luau.Window where
-- functions to help manipulate windows
import Prelude()
import UPrelude
import Elem.Data ( WinElem(..) )
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
    where w'    = w { winPages = pages }
          pages = addElemToPage page el (winPages w)

-- | adds an elem into a page
addElemToPage ∷ String → WinElem → [Page] → [Page]
addElemToPage _    _  []     = []
addElemToPage name el (p:ps)
  | name ≡ pageTitle p = [p'] ⧺ addElemToPage name el ps
  | otherwise          = [p]  ⧺ addElemToPage name el ps
    where p'    = p { pageElems = pageElems p ⧺ [el] }
