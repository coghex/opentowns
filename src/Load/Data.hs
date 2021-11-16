-- | data for the load thread, including queue commands
module Load.Data where
-- data for the loading thread is found
import Prelude()
import UPrelude
import Data ( Color (..), PrintArg(..), FPS(..), Shell(..), Popup(..) )
import Elem.Data ( WinElem(..) )
import Luau.Data ( Window(..), Page(..) )

-- | result of the loading thread
data LoadResult = ResSuccess | ResError String
                | ResDrawState DrawState | ResNULL

-- | commands that can be asked of the loading thread queue
data LoadCmd = LoadCmdPrint !PrintArg
             | LoadCmdVerts
             | LoadCmdDyns
             | LoadCmdNewWin !Window
             | LoadCmdNewPage !String !Page
             | LoadCmdNewElem !String !String !WinElem
             | LoadCmdSwitchWin !String
             | LoadCmdWindowSize !(Int,Int)
             | LoadCmdTest
             | LoadCmdNULL deriving (Show, Eq)

-- | the drawstate is converted into verticies and indicies
--   in the load thread. abstraction layer between Elem and Vulk
data DrawState = DrawState
  { dsStatus    ∷ DSStatus -- ^ return status for thread
  -- | abstract tiles contain 
  , dsTiles     ∷ [Tile]
  -- | each buffer holds a collection of dynamic data
  --   to be combined and converted to verticies
  , dsBuff      ∷ [Dyns]
  -- | frames per seconds has multiple values for PID-style corrections
  , dsFPS       ∷ FPS
  -- | windows are abstract seperate sets of verticies
  , dsWins      ∷ [Window]
  -- | popups are abstract verticies that are seperate from windows
  , dsPopup     ∷ [Popup]
  -- | the shell is completely seperate from all windowing
  , dsShell     ∷ Shell
  -- | this helps keep things centered to the corner for resizes
  , dsOldSize   ∷ Maybe (Int,Int) }

-- | a shortened draw state that gets used to pass around values
--   since the dyns buffer is quite big, like the 'identity'
data DrawStateP = DrawStateP { dspStatus    ∷ DSStatus
-- for example here we store ints instead of massive buffers
                             , dspBuffSizes ∷ [Int]
                             , dspFPS       ∷ FPS }

-- | status of the loading thread, allowing
--   us to return results of deeply nested
--   pure functions
data DSStatus = DSSLogDebug Int String
              | DSSRecreate
              | DSSReload
              | DSSLoadVerts
              | DSSLoadDyns
 --             | DSSLoadCap Bool
 --             | DSSSwitchWin String
              | DSSExit
              | DSSNULL deriving (Show, Eq)

-- | gtiles represent abstract tiles
data Tile = GTile { tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tT     ∷ Int
                  , tColor ∷ Color }
-- dtiles change data dynamically
          | DTile { tDyn   ∷ DynMap
                  , tPos   ∷ (Double,Double)
                  , tScale ∷ (Double,Double)
                  , tInd   ∷ (Int,Int)
                  , tSize  ∷ (Int,Int)
                  , tMoves ∷ Bool
                  , tT     ∷ Int
                  } deriving (Show, Eq)

-- | a collection of dyndatas, hlint suggest newtype here
newtype Dyns = Dyns [DynData] deriving (Show, Eq)

-- | data for dynamic object transformations
data DynData = DynData { ddPos     ∷ (Float,Float)
                       , ddScale   ∷ (Float,Float)
                       , ddTex     ∷ Int
                       , ddTIndex  ∷ (Int,Int)
                       } deriving (Show, Eq)

-- | mapping of buffer to tiles, this could be used
--   to hold all different kinds of things at the lowest
--   level but right now its just for generic buffers
data DynMap = DMBuff Int Int
            | DMNULL deriving (Show, Eq)
