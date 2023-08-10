{-# LANGUAGE StrictData #-}
-- | initialization of many things, including state,
--   env, drawsstate, inputsttate, settings, and
--   the keymapping, among others...
module Prog.Init
  ( runProg, initDrawState, initGameState
  , initInpState, initKeyMap ) where
-- initialization of env and state occurs
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data ( Color(..) )
import qualified Data.Map as Map
import Data.Time.Clock.System ( getSystemTime )
import qualified HsLua as Lua
import GHC.Stack ( HasCallStack) -- , prettyCallStack, callStack )
import Elem.Data ( CapType(..) )
import Load.Data ( DSStatus(DSSNULL), DrawState(DrawState), WinsState(..)
                 , GameState(..), GSStatus(GSSNULL), Buff(..), Dyns(..)
                 , BuffIndex(..), DynData(..) )
import Data
    ( Key(..), KeyFunc(..), LoadState(..),
      KeyMap(..), Difficulty(..),
      FPS(..), Shell(..), MapType(..), MapTile(..), MapTiles(..) )
import Prog ( Prog(unProg) )
import Prog.Data
    ( Env(..), Halt(..),
      ISKeys(..),
      ISStatus(ISSNULL),
      InputState(..),
      ProgResult(ProgSuccess),
      ReloadState(RSNULL),
      Settings(Settings),
      State(..) )
import Sign.Except ( ExType(ExProg), ProgExcept(ProgExcept) )
import Sign.Queue ( newQueue, newTChan )
import Sign.Var ( atomically, newTVar, TVar )

-- | the entire monad is unraveled here, after the init functions
runProg ∷ HasCallStack ⇒ (Either ProgExcept α → IO σ)
  → Prog ε σ α → IO σ
runProg c p = do
  (envchan,env) ← initEnv
  st            ← initState env
  unProg p envchan st c

-- | read-only env is best for channels, queues, and other pointers
initEnv ∷ IO (TVar Env, Env)
initEnv = do
  -- event queues handles events from main thread, the
  -- event thread commands are for the main draw thread
  eventQ   ← newQueue
  -- load queue calculates then delivers verticies/indicies
  -- to the draw thread from the drawState
  loadQ    ← newQueue
  -- game thread loads game assets and tracks game state
  gameQ    ← newQueue
  -- input thread tracks the mouse and processes input
  inpQ     ← newQueue
  -- lua thread processes lua commands every arbitrary tick
  --- luaQ     ← newQueue
  -- lua state is only a reference so we keep it here
  luaState ← Lua.newstate
  -- channels that contain semaphores for each thread
  loadCh   ← newTChan
  gameCh   ← newTChan
  inputCh  ← newTChan
  luaCh    ← newTChan
  -- font metrics var to hold the current fonts metrics
  fontM    ← atomically $ newTVar Nothing
  -- a channel for the glfw window so the threads can get glfw info
  win      ← atomically $ newTVar Nothing
  -- vert TVar keeps verticies in a cache so when we only
  -- recalculate if we explicitly ask for it
  verts    ← atomically $ newTVar Nothing
  -- same for dynamic data, there will be lots of it
  dyns     ← atomically $ newTVar Nothing
  -- simple camera tvar
  cam      ← atomically $ newTVar Nothing
  let env = Env { envEventQ = eventQ
                , envLoadQ  = loadQ
                , envGameQ  = gameQ
                , envLoadCh = loadCh
                , envLuaCh  = luaCh
                , envLuaSt  = luaState
                , envInpQ   = inpQ
                , envInpCh  = inputCh
                , envGameCh = gameCh
                , envWindow = win
                , envFontM  = fontM
                , envCam    = cam
                , envVerts  = verts
                , envDyns   = dyns }

  -- and env that can be accessed transactionally
  envChan ← atomically $ newTVar env
  -- we return both so that initState doesnt need to load the TVar
  return (envChan, env)

-- | state is bes for things that change often and need to be accessed
--   often verts and dyns, and some other more time critical things are
--   kepts as transactional data referenced in the env.
initState ∷ Env → IO (TVar State)
initState _   = do
  -- the status handles errors
  let ref = ProgExcept (Just ProgSuccess) ExProg ""
  -- initial input state is empty
      is  = initInpState
  -- the logger provides multiple levels of warnings/info
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- the system time marks the start of execution
  st ← getSystemTime
  -- TODO: load settings from file
  -- default settings get loaded at start
  settings ← initSettings
  -- state is accessed transactionally
  atomically $ newTVar State { stStatus   = ref
                             , stLogFunc  = lf
                             , stWindow   = Nothing
                             , stReload   = RSNULL
                             , stNDefTex  = 0
                             , stSettings = settings
                             , stStartT   = st
                             , stWinSize  = (0,0)
                             , stFPS      = FPS 60.0 60 True
                             , stInput    = is
                             , stTick     = Nothing }

-- | settings begin with some dummy values atm
-- TODO: load settings from a file
initSettings ∷ IO Settings
initSettings = return $ Settings initKeyMap True 100 True 100 True
               False True False False 0 DNormal False False True 2
               $ Just 60

-- | creates a drawstate with empty values
initDrawState ∷ DrawState
initDrawState = DrawState DSSNULL [] initBuff (FPS 60.0 60 True)
  [] initWinsState [] initShell (0,0,-1) Nothing

-- | creates a shell with empty values
initShell ∷ Shell
initShell = Shell "$> " Nothing 1 "" "" "" "" False (-1) []

-- | creates a basic buff with the default buffers
initBuff ∷ Buff
initBuff = Buff $ Map.fromList [(BuffLink      ,initDyns 64)
                               ,(BuffButt      ,initDyns 64)
                               ,(BuffText      ,initDyns 512)
                               ,(BuffPopup     ,initDyns 64)
                               ,(BuffPUText    ,initDyns 256)
                               ,(BuffMap       ,initDyns 32)
                               ,(BuffLoadScreen,initDyns 32)]
-- | creates a dyns of empty dyndata
initDyns ∷ Int → Dyns
initDyns n = Dyns $ take n $ repeat
               $ DynData (0,0) (1,1) 0 (0,0) (Color 0 0 0 0)

-- | creates an empty input state
initInpState ∷ InputState
initInpState = InputState { inpStatus = ISSNULL
                          , mouse1    = Nothing
                          , mouse2    = Nothing
                          , mouse3    = Nothing
                          , mousePos  = (0,0)
                          , isElems   = []
                          , isWin     = "win1"
                          , isPage    = "menu1"
                          , isHalt    = HaltNULL
                          , inpCap    = CapNULL
                          , accelCap  = False
                          , keySt     = initKS }
    where initKS = ISKeys { keyUp     = False
                          , keyLeft   = False
                          , keyDown   = False
                          , keyRight  = False
                          , keyAccel  = (0,0) }

-- | an empty winsstate
initWinsState ∷ WinsState
initWinsState = WinsState [] Unloaded

-- | inits the state for the game thread
initGameState ∷ GameState
initGameState = GameState GSSNULL maptiles MapNULL
  where maptiles = MapTiles (0,0) [[[MapTile 0 0]]]

-- | creates the base key mapping
-- TODO: load this from a file
initKeyMap ∷ KeyMap
initKeyMap = KeyMap $ Map.fromList
  [(KFEscape,[KeyEscape])
  ,(KFReturn,[KeyReturn])
  ,(KFScrollUp,[KeyW,KeyUp])
  ,(KFScrollDown,[KeyS,KeyDown])
  ,(KFScrollLeft,[KeyA,KeyLeft])
  ,(KFScrollRight,[KeyD,KeyRight])
  ,(KFShell,[KeyTilde])
  ,(KFFullScreen,[KeyF11])
  ,(KFTest,[KeyT]), (KFTest2,[KeyI])]

