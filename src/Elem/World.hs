module Elem.World where
-- the world map is calculated here
import Prelude()
import UPrelude
import Data ( Color(..) )
import Elem.Data ( WinElem )
import Load.Data ( DynData(..) )

genWorldDyns ∷ WinElem → [DynData]
genWorldDyns we = [DynData (0,0) (10,4) 107 (4,2) (Color 1 1 1 1)]
