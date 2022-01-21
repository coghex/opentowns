-- | functions related to loading lua into load thread, using
--   a seperate game thread that can keep game state computationally
--   seperated from the lua and draw threads
module Load.Game where
-- we load up data from disk, generate maps,
-- ui, and start up the game thread
import Prelude()
import UPrelude
import Control.Concurrent ( threadDelay )
import Control.Monad.IO.Class ( liftIO )
import System.Log.FastLogger (LogType'(..))
import Data.Time.Clock ( getCurrentTime, diffUTCTime )
import Data ( LoadState(..) )
import Load.Data ( DrawState(..), WinsState(..), DSStatus(..)
                 , DrawStateCmd(..), LoadCmd(..), LoadResult(..)
                 , GameState(..), GameCmd(..) )
import Prog.Data ( Env(..) )
import Prog.Init ( initGameState )
import Sign.Data ( LogLevel(..), TState(..) )
import Sign.Log

-- | starts the game thread
genGame ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → LogT μ DrawState
genGame ds = do
  sendGameCmd GameCmdStart
  let ds'   = ds { dsWinsState = ws { loading = newLS }
               , dsStatus    = DSSRecreate }
      ws    = dsWinsState ds
      newLS = Loading
  return ds'

-- | game thread
gameThread ∷ Env → IO ()
gameThread env = do
  logger ← makeDefaultLogger env (LogStdout 4096) (LogDebug 2)
  runLog logger $ runGameLoop initGS TStop
  where initGS = initGameState
runGameLoop ∷ (MonadLog μ, MonadFail μ) ⇒ GameState → TState → LogT μ ()
runGameLoop gs TStop = do
  tsNew ← readGameTimerBlocked
  runGameLoop gs tsNew
runGameLoop gs TStart = do
  start ← liftIO getCurrentTime
  timerState ← readGameTimer
  tsNew ← case timerState of
    Nothing → return TStart
    Just x  → return x
  gs' ← processCommands gs
  end ← liftIO getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 100000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then liftIO $ threadDelay delay
    else return ()
  runGameLoop gs' tsNew
runGameLoop _ TPause = return ()
runGameLoop _ TNULL  = return ()

processCommands ∷ (MonadLog μ,MonadFail μ) ⇒ GameState → LogT μ GameState
processCommands gs = do
  mcmd ← readGameCommand 
  case mcmd of
    Just cmd → do
      ret ← processCommand gs cmd
      case ret of
        ResSuccess → processCommands gs
        ResDrawState _ → do
          log' LogWarn "game thread cant process load result"
          processCommands gs
        ResError str → do
          log' LogError $ "game command error: " ⧺ str
          processCommands gs
        ResNULL → do
          log' LogInfo "load null command"
          return gs
    Nothing → return gs

-- | this is the case statement for processing game commands
processCommand ∷ (MonadLog μ,MonadFail μ)
  ⇒ GameState → GameCmd → LogT μ LoadResult
processCommand gs cmd = case cmd of
  GameCmdStart → do
    log' LogInfo "game start"
    liftIO $ threadDelay 5000000
    sendLoadCmd $ LoadCmdDS DSCLoadMap
    return ResSuccess
  GameCmdNULL  → return ResNULL
