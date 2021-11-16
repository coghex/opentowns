-- | lua commands that are registered in Luau.hs are defined here.
module Luau.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Load.Data ( LoadCmd(..) )
import Prog.Data ( Env(envEventQ, envLoadQ) )
import Sign.Data
    ( Event(EventSys, EventLog),
      LogLevel(LogError, LogDebug, LogInfo),
      SysAction(SysReload, SysExit, SysRecreate) )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically )
import Luau.Data ( Window(Window), Page(..) )

-- | quits everything using glfw
hsExit ∷ Env → Lua.Lua ()
hsExit env = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventSys SysExit

-- | logs at level n, 1 being -v, 3 being -vvv,
--   0 being no verbosity whatsoever
hsLogDebug ∷ Env → Int → String → Lua.Lua ()
hsLogDebug env n str = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventLog (LogDebug n) str

-- | logs info, should not be used in production code
hsLogInfo ∷ Env → String → Lua.Lua ()
hsLogInfo env str = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventLog LogInfo str

-- | logs a string and ends the entire process and children
hsLogError ∷ Env → String → Lua.Lua ()
hsLogError env str = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventLog LogError str

-- | recreates the swapbuffer
hsRecreate ∷ Env → Lua.Lua ()
hsRecreate env = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventSys SysRecreate

-- | recalculates and loads dynamic data into
--   the vulkan transition functions
hsReload ∷ Env → Lua.Lua ()
hsReload env = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventSys SysReload

-- | adds a new window to the draw state
hsNewWindow ∷ Env → String → Lua.Lua()
hsNewWindow env name = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewWin win
  -- TODO: unhardcode the window size
  where win = Window name (1280,720) [] [] "NULL"

-- | add a new page to the draw state
hsNewPage ∷ Env → String → String → Lua.Lua()
hsNewPage env name pname = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewPage name page
  where page = Page pname []

-- | switches to page by name
hsGoToPage ∷ Env → String → Lua.Lua()
hsGoToPage env name = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdSwitchWin name
