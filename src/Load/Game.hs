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
import Data ( LoadState(..), MapSettings(..)
            , BuriedStatus(..), MapType(..) )
import Elem.Data ( WinElem(..) )
import Load.Data ( DrawState(..), WinsState(..), DSStatus(..)
                 , DrawStateCmd(..), LoadCmd(..), LoadResult(..)
                 , GameState(..), GSStatus(..), GameCmd(..)
                 , BuffIndex(..) )
import Load.Map ( genMapData )
import Luau.Data ( Window(..), Page(..) )
import Prog.Data ( Env(..) )
import Prog.Init ( initGameState )
import Sign.Data ( LogLevel(..), TState(..) )
import Sign.Log

-- | starts the game thread
genGame ∷ (MonadLog μ, MonadFail μ) ⇒ DrawState → LogT μ DrawState
genGame ds = do
  let ds'       = ds { dsWinsState = ws { loading = newLS }
                     , dsStatus    = DSSReload }
      ws        = dsWinsState ds
      newLS     = Loading
      ms        = findMapSettings $ dsWins ds
  sendGameCmd $ GameCmdStart ms
  return ds'

-- | pulls the map settings out of a drawstate's map element
findMapSettings ∷ [Window] → MapSettings
findMapSettings []     = MapSettings NoBuried MapNULL (0,0)
findMapSettings (w:ws) = case findPageMapSettings (winPages w) of
  Just ms0 → ms0
  Nothing  → findMapSettings ws
findPageMapSettings ∷ [Page] → Maybe MapSettings
findPageMapSettings []     = Nothing
findPageMapSettings (p:ps) = case findElemsMapSettings (pageElems p) of
  Just ms0 → Just ms0
  Nothing  → findPageMapSettings ps
findElemsMapSettings ∷ [WinElem] → Maybe MapSettings
findElemsMapSettings []     = Nothing
findElemsMapSettings (e:es) = case findElemMapSettings e of
  Just ms0 → Just ms0
  Nothing  → findElemsMapSettings es
findElemMapSettings ∷ WinElem → Maybe MapSettings
findElemMapSettings (WinElemMap ms0 _) = Just ms0
findElemMapSettings _                  = Nothing

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
        ResGameState gs' → case gsStatus gs' of
          GSSLogDebug n str → do
            log' (LogDebug n) str
            processCommands gs''
              where gs'' = gs' { gsStatus = GSSNULL }
          GSSNULL → processCommands gs'
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
  GameCmdStart msettings → do
    log' LogInfo "game start"
    let gs' = gs { gsMapData = tiles }
        tiles = genMapData msettings
    liftIO $ threadDelay 1000000
    sendLoadCmd $ LoadCmdNewBuff BuffMap 1024
    sendLoadCmd $ LoadCmdDS $ DSCLoadMap tiles
    return $ ResGameState gs'
  GameCmdNULL  → return ResNULL
