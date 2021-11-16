-- | winelem data structures, highest non-lua level of abstract
module Elem.Data where
-- high level structures for different types of UI elements
import Prelude()
import UPrelude
import Data ( Color(..) )

-- |  various win elements and their associated data
data WinElem
  = WinElemText { textPos   ∷ (Double,Double)
                , textColor ∷ Color
                , textStr   ∷ String }
  | WinElemNULL deriving (Show, Eq)
