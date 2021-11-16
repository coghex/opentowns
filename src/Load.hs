{-# LANGUAGE Strict #-}
-- | a huge portion of the engine is here in the load thread.
--   any time a change needs to occur on screen, the abstract
--   representation of the draw state needs to be calculated
--   into dynamic data and passed to the main draw thread. any
--   changes to the number of objects on screen will trigger
--   this thread to also generate the verticies and indicies
module Load where
-- a thread to help recreate the swapchain
import Prelude()
import UPrelude
import Data ( PrintArg(PrintNULL) )
import Data.Maybe ( fromMaybe )
import Load.Data
    ( DSStatus(..),
      DrawState(dsStatus, dsTiles, dsBuff, dsWins),
      LoadCmd(..),
      LoadResult(..) )
import Luau.Data ()
import Luau.Window ( addPageToWin, addElemToPageInWin, currentWin )
import Prog.Buff ( genDynBuffs, loadDyns, initBuff )
import Prog.Data ( Env(envLoadCh, envFontM, envLoadQ, envEventQ) )
import Sign.Data
    ( LoadData(LoadDyns, LoadVerts),
      LogLevel(..), SysAction(..), TState(..) )
import Sign.Log
import Sign.Var ( atomically, readTVar )
import Sign.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Prog.Init ( initDrawState )
import Vulk.Calc ( calcVertices )
import Vulk.Data ( Verts(Verts) )
import Vulk.Draw ( loadTiles )
import Vulk.Mouse ()
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( liftIO, MonadIO(..) )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Vulk.GLFW as GLFW
import System.Log.FastLogger (LogType'(LogStdout))

-- | threaded loop provides work so main thread doesnt stutter
loadThread ∷ Env → GLFW.Window → IO ()
loadThread env win = do
  logger ← makeDefaultLogger env (LogStdout 4096) (LogDebug 2)
--  runLog logger $ log' LogInfo "asdf"
  runLog logger $ runLoadLoop win initDS TStop
  where initDS = initDrawState
-- | timed loop so that its not running full speed all the time
runLoadLoop ∷ (MonadLog μ,MonadFail μ) ⇒ GLFW.Window → DrawState → TState → LogT μ ()
runLoadLoop win ds TStop = do
  -- loop starts almost immediately
  tsNew ← readTimerBlocked
  runLoadLoop win ds tsNew
runLoadLoop win ds TStart = do
  start ← liftIO getCurrentTime
  timerState ← readTimer
  tsNew ← case timerState of
    Nothing → return TStart
    Just x  → return x
  ds' ← processCommands win ds
  end ← liftIO getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then liftIO $ threadDelay delay
    else return ()
  runLoadLoop win ds' tsNew
-- pause not needed for this timer
runLoadLoop _   _ TPause = return ()
runLoadLoop _   _ TNULL  = return ()

-- | command queue processed once per loop
processCommands ∷ (MonadLog μ,MonadFail μ) ⇒ GLFW.Window → DrawState → LogT μ DrawState
processCommands win ds = do
  mcmd ← readCommand
  case mcmd of
    Just cmd → do
      ret ← processCommand win ds cmd
      case ret of
        -- if command success keep processing commands
        ResSuccess       → processCommands win ds
        -- request to change the draw state
        ResDrawState ds' → case dsStatus ds' of
          DSSNULL → processCommands win ds'
          DSSExit → do
            sendSys SysExit
            return ds'
          DSSLoadDyns → do
            sendLoadCmd LoadCmdDyns
            processCommands win ds''
              where ds'' = ds' { dsStatus = DSSNULL }
          DSSLoadVerts → do
            sendLoadCmd LoadCmdVerts
            processCommands win ds''
              where ds'' = ds' { dsStatus = DSSNULL }
          DSSRecreate → do
            sendSys SysRecreate
            processCommands win ds''
              where ds'' = ds' { dsStatus = DSSNULL }
          DSSReload → do
            sendSys SysReload
            processCommands win ds''
              where ds'' = ds' { dsStatus = DSSNULL }
          DSSLogDebug n str → do
            log' (LogDebug n) str
            processCommands win ds''
              where ds'' = ds' { dsStatus = DSSNULL }
        ResError str → do
          log' LogError $ "load command error: " ⧺ str
          processCommands win ds
        ResNULL → do
          log' LogInfo "load null command"
          return ds
    Nothing → return ds

-- | this is the case statement for processing load commands
processCommand ∷ (MonadLog μ,MonadFail μ)
  ⇒ GLFW.Window → DrawState → LoadCmd → LogT μ LoadResult
processCommand glfwwin ds cmd = case cmd of
  -- context sensitive print
  LoadCmdPrint arg → do
    log' (LogDebug 3) "LoadCmdPrint"
    let ret = case arg of
                PrintNULL → "print null command"
    log' LogInfo ret
    return ResSuccess
  LoadCmdVerts → do
    log' (LogDebug 3) "LoadCmdVerts"
    ttfdat' ← readFontMapM
    (w',h') ← liftIO $ GLFW.getWindowSize glfwwin
    let ttfdat   = fromMaybe [] ttfdat'
        winSize  = (fromIntegral w'/64.0,fromIntegral h'/64.0)
        newVerts = Verts $ calcVertices $ loadTiles ds winSize ttfdat
        ds'      = ds { dsTiles = loadTiles ds winSize ttfdat }
    sendLoad $ LoadVerts newVerts
    -- TODO: test if the next line is neccesary
    sendLoadCmd LoadCmdDyns
    return $ ResDrawState ds'
  LoadCmdDyns → do
    log' (LogDebug 3) "LoadCmdDyns"
    ttfdat' ← readFontMapM
    let newDyns = loadDyns ds'
        ds'     = ds { dsBuff = genDynBuffs ttfdat ds }
        ttfdat  = fromMaybe [] ttfdat'
    sendLoad $ LoadDyns newDyns
    return $ ResDrawState ds'
  LoadCmdInitBuff tiles → do
    return $ ResDrawState $ ds { dsTiles = tiles
 --                              , dsBuff  = initBuff $ case currentWin (dsWins ds) of
                               , dsBuff  = initBuff [64,64,256,64,256,256] }
        --                         Nothing → []
        --                         Just w0 → winBuffs w0 }
  LoadCmdNewWin win → do
    log' (LogDebug 3) "LoadCmdNewWin"
    return $ ResDrawState ds'
    where ds' = ds { dsWins = win:dsWins ds }
  LoadCmdNewPage win page → do
    log' (LogDebug 3) "LoadCmdNewPage"
    return $ ResDrawState ds'
    where ds' = ds { dsWins = addPageToWin win page (dsWins ds) } 
  LoadCmdNewElem win page el → do
    log' (LogDebug 3) "LoadCmdNewElem"
    return $ ResDrawState ds'
    where ds' = ds { dsWins = addElemToPageInWin win page el (dsWins ds) }
  LoadCmdSwitchWin _   → do
    log' (LogDebug 3) "LoadCmdSwitchWin"
    let ds' = ds
    --let ds' = ds { dsWins      = switchWin win (dsWins ds) }
    --    buffSizes = case (findWin win (dsWins ds)) of
    --                  Nothing → []
    --                  Just w  → winBuffs w
    sendLoadCmd LoadCmdVerts
    --atomically $ writeQueue (envInpQ  env) $ InputSwitchWin win
    return $ ResDrawState ds'
  -- sometimes you need to test something with a command
  LoadCmdWindowSize _ → return ResSuccess
  LoadCmdTest → do
    log' (LogDebug 2) $ show $ dsWins ds
    return ResSuccess
  LoadCmdNULL → return ResNULL