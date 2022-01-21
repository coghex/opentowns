-- | data for the load thread, including queue commands
module Load.Data where
-- data for the loading thread is found
import Prelude()
import UPrelude
import Data ( Color (..), PrintArg(..), FPS(..), LoadState(..)
            , Shell(..), Popup(..), PopupType(..), KeyFunc(..)
            , Key(..), MapType(..) )
import Elem.Data ( WinElem(..), Button(..), InputAct(..) )
import Luau.Data ( Window(..), Page(..) )

-- | result of the loading thread
data LoadResult = ResSuccess | ResError String
                | ResDrawState DrawState | ResNULL

-- | commands that can be asked of the loading thread queue
data LoadCmd = LoadCmdPrint !PrintArg
             | LoadCmdVerts
             | LoadCmdDyns
             | LoadCmdInitBuff ![Tile]
             | LoadCmdNewWin !Window
             | LoadCmdNewPage !String !Page
             | LoadCmdNewElem !String !String !WinElem
             | LoadCmdSwitchWin !String !String
             | LoadCmdWindowSize !(Int,Int)
             | LoadCmdDS !DrawStateCmd
             | LoadCmdInput InputAct
             | LoadCmdGame
             | LoadCmdTest
             | LoadCmdTest2
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
  -- | current and last window are saved,
  --   along with current and last page
  --   and a loading screen state
  --, dsWinsState ∷ ((String,String),(String,String))
  , dsWinsState ∷ WinsState
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
              | DSSLoadScreen
              | DSSExit
              | DSSNULL deriving (Show, Eq)

-- | draw state commands are the enumeration of commands to change
--   the draw state, if these were enumerated as LoadCmds, the Load.hs
--   file would be huge, so we process them seperately in Load.Cmd
data DrawStateCmd = DSCToggleButts [Button] Bool
                  | DSCUpdatePopup PopupType
                  | DSCClearPopup PopupType
                  | DSCUpdateKeyButton KeyFunc [Key]
                  | DSCSavename String
                  | DSCLoadMap
                  | DSCNULL deriving (Show, Eq)

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
                       , ddColor   ∷ Color
                       } deriving (Show, Eq)

-- | mapping of buffer to tiles, this could be used
--   to hold all different kinds of things at the lowest
--   level but right now its just for generic buffers
data DynMap = DMBuff Int Int
            | DMNULL deriving (Show, Eq)

-- | a collection of memory for the state of the windowing
data WinsState = WinsState { thisWin  ∷ String
                           , lastWin  ∷ String
                           , thisPage ∷ String
                           , lastPage ∷ String
                           , loading  ∷ LoadState
                           , loadStr  ∷ String } deriving (Show, Eq)

-- | state of the game thread
data GameState = GameState { gsStatus  ∷ GSStatus
                           , gsMapType ∷ MapType }
-- | return state of game thread loop
data GSStatus = GSSLogDebug Int String
              | GSSNULL deriving (Show, Eq)

-- | possible commands to the game thread
data GameCmd = GameCmdStart
             | GameCmdNULL deriving (Show, Eq)
