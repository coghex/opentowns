-- | windows are abstract representations of glfw windows,
--   filled with abstract 'pages' which hold 'elems'.
--   pages are like different screens, elems are like
--   individual text lines or buttons...
module Luau.Window where
-- functions to help manipulate windows
import Prelude()
import UPrelude
import Luau.Data ( Window )

-- | returns maybe the head window
currentWin ∷ [Window] → Maybe Window
currentWin wins
  | length wins ≤ 0 = Nothing
  | otherwise       = Just $ head wins

