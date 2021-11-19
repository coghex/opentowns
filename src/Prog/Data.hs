-- | data for the continuation monad
module Prog.Data where
-- the state is
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data ( FPS(..), Difficulty(..)
            , Key, KeyFunc, KeyMap )
import Data.Time.Clock.System ( SystemTime )
import qualified Foreign.Lua as Lua
import Elem.Data ( Button(..) )
import Load.Data ( LoadCmd(..), Dyns(..) )
import Sign.Data ( Event, TState )
import Sign.Except ( ProgExcept )
import Sign.Queue ( Queue, TChan )
import Sign.Var ( TVar )
import Vulk.Data ( Verts )
import Vulk.Font ( TTFData )
import qualified Vulk.GLFW as GLFW

-- | specific utility actions
data ProgResult = ProgSuccess | ProgError deriving (Show, Eq)
-- | glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq

-- | env should only hold pointers/references
data Env = Env { envEventQ ∷ Queue Event
               , envLoadQ  ∷ Queue LoadCmd
               , envLoadCh ∷ TChan TState
               , envLuaCh  ∷ TChan TState
               , envLuaSt  ∷ Lua.State
               , envInpCh  ∷ TChan TState
               , envInpQ   ∷ Queue InputAct
               , envFontM  ∷ TVar (Maybe [TTFData])
               , envVerts  ∷ TVar (Maybe Verts)
               , envDyns   ∷ TVar (Maybe Dyns) }

-- | state holds mutable data, and the
--   current status of the whole App
data State = State { stStatus   ∷ ProgExcept
                   -- logging monadic function
                   , stLogFunc  ∷ Logger.Loc → Logger.LogSource
                                    → Logger.LogLevel → Logger.LogStr
                                    → IO ()
                   -- | the main glfw object
                   , stWindow   ∷ !(Maybe GLFW.Window)
                   -- | reloads or recreates the swapchain when needed
                   , stReload   ∷ !ReloadState
                   -- | number of textures added on top of default
                   , stNDefTex  ∷ !Int
                   -- | user settings
                   , stSettings ∷ !Settings
                   -- | user inputs kept strictly
                   , stInput    ∷ !InputState
                   -- | variables for FPS calculation
                   , stStartT   ∷ !SystemTime
                   , stFPS      ∷ !FPS
                   , stTick     ∷ !(Maybe Double) }

-- | defines if we want to reload everything and how
data ReloadState = RSReload | RSRecreate | RSNULL deriving (Show, Eq)

-- | defines some user alterable settings
data Settings = Settings { sKeyLayout            ∷ KeyMap
                         , sMusic                ∷ Bool
                         , sMusicVol             ∷ Int
                         , sFX                   ∷ Bool
                         , sFXVol                ∷ Int
                         , sMouseScroll          ∷ Bool
                         , sEdgeScroll           ∷ Bool
                         , sHeightCube           ∷ Bool
                         , sItemsDisabledDefault ∷ Bool
                         , sPauseGameStart       ∷ Bool
                         , sAutosave             ∷ Int
                         , sSieges               ∷ Difficulty
                         , sSiegePause           ∷ Bool
                         , sCaravanPause         ∷ Bool
                         , sBurySystem           ∷ Bool
                         , sCPUUsageLevel        ∷ Int
                         , sFPSCap               ∷ Maybe Int }

-- | input state is for the main thread only
data InputState = InputState { inpStatus ∷ ISStatus
                             , mouse1    ∷ Maybe (Double,Double)
                             , mouse2    ∷ Maybe (Double,Double)
                             , mouse3    ∷ Maybe (Double,Double)
                             , mousePos  ∷ (Double,Double)
                             , isElems   ∷ [InputElem]
                             , isHalt    ∷ Halt
                             , isWin     ∷ String
                             , isPage    ∷ String
                             , inpCap    ∷ CapType
                             , accelCap  ∷ Bool
                             , keySt     ∷ ISKeys
                             } deriving (Show, Eq)
-- | return status for the input thread
data ISStatus = ISSLogDebug String
              | ISSNULL deriving (Show, Eq)

-- | possible return types
data InpResult = ResInpSuccess | ResInpError String
               | ResInpState InputState
               | ResInpChangeKey KeyFunc Key Int | ResInpNULL

-- | various situations in which a key may be captured
data CapType = CapKeyChange Int KeyFunc
             | CapNULL deriving (Show, Eq)

-- | various situations in which a bit of data needs to be held
--   for caching and toggle functionality
data Halt = HaltButton Bool
          | HaltNULL deriving (Show, Eq)

-- | input state related to various winelems
data InputElem = IEButt Button | IENULL deriving (Show, Eq)
-- | possible input actions
data InputAct = InpActKey GLFW.Key GLFW.KeyState GLFW.ModifierKeys 
              | InpActMouse GLFW.MouseButton
                  GLFW.MouseButtonState GLFW.ModifierKeys
              | InpActSwitchWin String
              | InpActSetCap CapType
              | InpActSetLink Button
              | InpActNULL deriving (Show, Eq)

-- | certain keys state
data ISKeys = ISKeys { keyUp    ∷ Bool
                     , keyLeft  ∷ Bool
                     , keyDown  ∷ Bool
                     , keyRight ∷ Bool
                     , keyAccel ∷ (Double,Double)
                     } deriving (Show, Eq)

