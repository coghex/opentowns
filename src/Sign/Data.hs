-- | various ADTs for handling events, input and load data
module Sign.Data where
-- data for the main event queue is defined
import Prelude()
import UPrelude
import Data ( PrintArg(..) )
import Elem.Data ( Button(..) )
import Load.Data ( Dyns(..) )
import Vulk.Data ( Verts(..) )
import qualified Vulk.GLFW as GLFW

-- | timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- | events processed by the main thread
data Event = EventError !GLFW.Error !String -- GLFW specific
           -- | logs into the monadic logger, but also allows stdout
           | EventLog !LogLevel !String
           -- | key/mouse input, mostly from GLFW callbacks
           | EventInput !InputEvent
           -- | verticies, indicies, and dynamic data from the load thread
           | EventLoad !LoadData
           -- dynamic changes to state act as settings
     --      | EventSettings !SettCmd
           -- | lowest level actions go here
           | EventSys !SysAction

-- | log levels are for monadic logger, but stdio
data LogLevel = LogDebug Int
              | LogInfo
              | LogWarn
              | LogPrint PrintArg
              | LogError
              | LogNULL deriving (Show, Eq)

-- | input sources enumerated
data InputEvent
  = InputKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState
      !GLFW.ModifierKeys
  | InputMouseButton !GLFW.Window !GLFW.MouseButton
      !GLFW.MouseButtonState !GLFW.ModifierKeys
  | InputMouseScroll !GLFW.Window !Double !Double

-- | data gets loaded in from a seperate thread
data LoadData = LoadVerts !Verts
              | LoadDyns !Dyns

-- | settings are changes to the state, some require
--   the parent thread, so those go here...
data SettCmd = SettCmdNULL deriving (Show, Eq)

-- | commands for functionality at the lowest level
data SysAction = SysRecreate | SysReload
               | SysExit | SysNULL deriving (Show, Eq)


