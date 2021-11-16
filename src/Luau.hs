-- | lua interpreter runs initLuau function from
--   each mod once at the beginning the runLuau
--   once for each mod every lua thread tick.
module Luau where
-- lua loader and interpreter
import Prelude()
import UPrelude
import Data.List (sort)
import Data.Maybe ( fromMaybe )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Foreign.Lua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Load.Data ( LoadCmd(LoadCmdDyns, LoadCmdVerts) )
import Luau.Command
import Prog.Data ( Env(envLoadQ, envEventQ, envLuaCh, envLuaSt) )
import Sign.Data
    ( Event(EventLog, EventSys),
      LogLevel(LogDebug),
      SysAction(SysRecreate),
      TState(..) )
import Sign.Thread (threadDelay)
import Sign.Queue (readChan, tryReadChan, writeQueue)
import Sign.Var (atomically)

-- | initialization of each mod file, as well as registering all
--   of the raw functions, and kickoff of the vertex generation
luauThread ∷ Env → IO ()
luauThread env = do
  modFiles ← findModFiles "mod/game/"
  if modFiles == [] then do
    let eventQ = envEventQ env
    atomically $ writeQueue eventQ
      $ EventLog (LogDebug 1) "src/Luau.hs: no files in mod/game/"
    return ()
  else do
    let ls = envLuaSt env
    _ ← Lua.runWith ls $ do
      Lua.registerHaskellFunction "rawExit"         (hsExit         env)
      Lua.registerHaskellFunction "rawNewWindow"    (hsNewWindow    env)
      Lua.registerHaskellFunction "rawNewPage"      (hsNewPage      env)
      Lua.registerHaskellFunction "rawNewElem"      (hsNewElem      env)
      Lua.registerHaskellFunction "rawGoToPage"     (hsGoToPage     env)
      Lua.registerHaskellFunction "logDebug"        (hsLogDebug     env)
      Lua.registerHaskellFunction "logInfo"         (hsLogInfo      env)
      Lua.registerHaskellFunction "logError"        (hsLogError     env)
      Lua.registerHaskellFunction "recreate"        (hsRecreate     env)
      Lua.registerHaskellFunction "reload"          (hsReload       env)
      Lua.openlibs
      _ ← Lua.dofile "mod/base/game.lua"
      ret ← Lua.callFunc "initLuau" modFiles
      return (ret∷Int)
    let loadQ = envLoadQ env
    atomically $ writeQueue (envEventQ env) $ EventSys SysRecreate
    atomically $ writeQueue loadQ LoadCmdVerts
    atomically $ writeQueue loadQ LoadCmdDyns
    luauLoop TStart env modFiles

-- | the loop runs lua commands every loop
luauLoop ∷ TState → Env → String → IO ()
luauLoop TPause env modFiles = do
  let timerChan = envLuaCh env
  atomically $ writeQueue (envEventQ env)
    $ EventLog (LogDebug 3) "starting luau loop..."
  tsNew ← atomically $ readChan timerChan
  luauLoop tsNew env modFiles
luauLoop TStart env modFiles = do
  let timerChan = envLuaCh env
  start ← getCurrentTime
  tsMby ← atomically $ tryReadChan timerChan
  let tsNew = fromMaybe TStart tsMby
      ls = envLuaSt env
  _ ← Lua.runWith ls $ do
    Lua.openlibs
    _ ← Lua.dofile "mod/base/game.lua"
    ret ← Lua.callFunc "runLuau" modFiles
    return (ret∷Int)
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = n*1000 - usecs
      n     = 1000
  if delay > 0 then threadDelay delay else return ()
  luauLoop tsNew env modFiles
luauLoop TStop _   _      = return ()
luauLoop TNULL _   _      = return ()

-- | simple utility function that may or may not work on windows
findModFiles ∷ String → IO String
findModFiles path = do
  paths ← getDirectoryContents "mod/game/"
  return $ collapsePaths $ map (combine path)
           $ sort $ filter filterOutPathJunk paths
  where filterOutPathJunk ∷ FilePath → Bool
        filterOutPathJunk "."  = False
        filterOutPathJunk ".." = False
        filterOutPathJunk _    = True
        collapsePaths ∷ [String] → String
        collapsePaths [] = ""
        collapsePaths [str]      = str
        collapsePaths (str:strs) = str ⧺ ";" ⧺ collapsePaths strs
