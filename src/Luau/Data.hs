-- | data structures for the lua interpreter
module Luau.Data where
-- data for lua interpreter
import Elem.Data ( WinElem(..) )

-- | abstract window
data Window = Window { winTitle  ∷ String
                     , winSize   ∷ (Int,Int)
                     , winPages  ∷ [Page]
                     } deriving (Show, Eq)

-- | each window contains pages, each page contains winElems
data Page = Page { pageTitle  ∷ String
                 , pageElems  ∷ [WinElem]
                 } deriving (Show, Eq)

