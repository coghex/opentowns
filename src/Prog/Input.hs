{-# LANGUAGE Strict #-}
-- | all input events are, in their callbacks, sending this thread
--   the input in the input queue.  it is then processed here
module Prog.Input where
-- a thread to handle input
import Prelude()
import UPrelude
import Data ( KeyFunc(..), KeyMap(..) )
import Elem.Data
    ( CapType(CapNULL, CapKeyChange),
      Button(..), ButtFunc(..),
      InputAct(..), InputElem(..) )
import Load.Data ( LoadCmd(..) )
import Prog.Data
    ( Env(..),
      ISStatus(ISSNULL, ISSLogDebug),
      InpResult(..),
      InputState(..) )
import Prog.KeyEvent ( changeKeyMap, findKey, lookupKey )
import Prog.Init ( initKeyMap, initInpState )
import Prog.Mouse ( processLoadMouse, findAllButtsUnder, findButts )
import Prog.Util ()
import Sign.Data
    ( Event(EventLog, EventSys),
      LogLevel(LogDebug, LogInfo),
      SysAction(SysExit),
      TState(..) )
import Sign.Var ( atomically )
import Sign.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Vulk.GLFW as GLFW

-- | threaded recursive loop
inputThread ∷ Env → GLFW.Window → IO ()
inputThread env win = do
  atomically $ writeQueue (envEventQ env)
    $ EventLog (LogDebug 2) "starting input thread..."
  runInputLoop env win initIS initKeyMap TStop
  where initIS = initInpState

-- | generic timed loop, so that the CPU can idle
runInputLoop ∷ Env → GLFW.Window → InputState → KeyMap → TState → IO ()
runInputLoop env win inpSt keyMap TStop = do
  let timerChan = envInpCh env
  tsNew ← atomically $ readChan timerChan
  runInputLoop env win inpSt keyMap tsNew
runInputLoop env win inpSt0 keyMap0 TStart = do
  start ← getCurrentTime
  let timerChan = envInpCh env
  timerState ← atomically $ tryReadChan timerChan
  tsNew ← case timerState of
            Nothing → return TStart
            Just x  → return x
  inpSt1 ← processLoadMouse env win inpSt0
  (inpSt2,keyMap1) ← processLoadInputs env win inpSt1 keyMap0
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runInputLoop env win inpSt2 keyMap1 tsNew
-- pause not needed for this timer
runInputLoop _ _ _ _ TPause = return ()
runInputLoop _ _ _ _ TNULL  = return ()

-- | this will recursively process a queue of inputs, allowing
--   each processing to return a result type, and then preforming
--   actions on that type
processLoadInputs ∷ Env → GLFW.Window → InputState → KeyMap
  → IO (InputState,KeyMap)
processLoadInputs env win inpSt keymap = do
  rawInp ← atomically $ tryReadQueue $ envInpQ env
  case rawInp of
    Just inp → do
      ret ← processLoadInput env win inpSt keymap inp
      case ret of
        -- if input is successful keep processing incoming input
        ResInpSuccess   → processLoadInputs env win inpSt keymap
        -- if we need to change input state
        ResInpState is' → case inpStatus is' of
          ISSNULL → processLoadInputs env win is' keymap
          ISSLogDebug str → do
            let eventQ = envEventQ env
            atomically $ writeQueue eventQ $ EventLog (LogDebug 1) str
            processLoadInputs env win is'' keymap
              where is'' = is' { inpStatus = ISSNULL }
        -- for the ingame changing of the keymap
        ResInpChangeKey keyFunc key 1 → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "new keys: " ⧺ show keymap'
          processLoadInputs env win inpSt' keymap'
            where keymap' = changeKeyMap keyFunc key 1 keymap
                  inpSt'  = inpSt { inpCap = CapKeyChange 2 keyFunc }
        ResInpChangeKey keyFunc key 2 → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "new keys: " ⧺ show keymap'
          processLoadInputs env win inpSt' keymap'
            where keymap' = changeKeyMap keyFunc key 2 keymap
                  inpSt'  = inpSt { inpCap = CapNULL }
        ResInpChangeKey keyFunc key n → do
          processLoadInputs env win inpSt keymap'
            where keymap' = changeKeyMap keyFunc key n keymap
        -- if we run across an error, we can return it here, print
        -- to the main thread and then continue to process input.
        -- the main event thread is responsible to check if we can
        -- continue to program or not
        ResInpError str    → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "input error: " ⧺ str
          processLoadInputs env win inpSt keymap
        -- null result will stop all processing for that frame,
        -- they actions will remain in the queue and should be
        -- processed next tick
        ResInpNULL → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) "input null command"
          return (inpSt,keymap)
    Nothing  → return (inpSt,keymap)

-- | indiviual input command processing
processLoadInput ∷ Env → GLFW.Window → InputState → KeyMap
  → InputAct → IO InpResult
processLoadInput env win inpSt keymap inp = case inp of
  -- key press action, if the keys have been captured by a
  -- window, we need to functionnn differently
  InpActKey k ks _  → case inpCap inpSt of
    -- input captured by key change pop up window
    CapKeyChange n keyFunc → if ks ≡ GLFW.KeyState'Pressed then do
      --  let k0 = km Map.! keyFunc
      --      (KeyMap km) = keymap
  --      atomically $ writeQueue (envLoadQ env)
  --        $ LoadCmdChangeKey n keyFunc $ if (n ≡ 1)
  --          then [(findKey k),last k0]
  --          else [head k0,(findKey k)]
        return $ ResInpChangeKey keyFunc (findKey k) n
      else return ResInpSuccess
    -- if no input capture, case on each key function
    CapNULL → if ks ≡ GLFW.KeyState'Pressed then case lookupKey keymap (findKey k) of
        KFEscape → do
          atomically $ writeQueue (envEventQ env) $ EventSys SysExit
          return ResInpSuccess
        KFTest → do
          atomically $ writeQueue (envLoadQ env) LoadCmdTest
          return ResInpSuccess
        KFTest2 → do
          atomically $ writeQueue (envLoadQ env) LoadCmdTest2
          return ResInpSuccess
        keyfunc → do
          atomically $ writeQueue (envEventQ env)
            $ EventLog (LogDebug 1) $ "unknown key " ⧺ show keyfunc
          return ResInpSuccess
      else return ResInpSuccess
-- for mouse button presses, note that the position and state
-- of the mouse are monitored in a completely different place,
-- but we have this simple GLFW interface we can use here too
  InpActMouse mb mbs mk
    → if ((mb ≡ GLFW.mousebutt1) ∧ not (GLFW.modifierKeysControl mk))
           ∧ (mbs ≡ GLFW.MouseButtonState'Pressed) then do
        pos   ← GLFW.getCursorPos win
        -- TODO: unhardcode the screen size here
        let butts = findAllButtsUnder (isWin inpSt) (isPage inpSt) (1280,720) (findButts (isElems inpSt)) pos
        if butts ≡ [] then return ResInpSuccess
        else case head butts of
          Button (ButtFuncLink _) _ _ _ _ → do
            atomically $ writeQueue (envInpQ env) $ InpActButton $ head butts
            atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ InpActButton $ head butts
            return ResInpSuccess
          _ → return ResInpSuccess
     --   print $ show pos
    else return ResInpSuccess
  InpActSetCap cap → return $ ResInpState inpSt'
    where inpSt' = inpSt { inpCap = cap }
  InpActSwitchWin w0  → return $ ResInpState inpSt'
    where inpSt' = inpSt { isWin = w0 }
  InpActSetLink butt → do
    return $ ResInpState inpSt'
    where inpSt' = inpSt { isElems = isElems inpSt ⧺ [IEButt butt] }
  -- InpActButton butt → do
    --print $ "button press: " ⧺ show butt
  InpActButton _         → return ResInpSuccess
  InpActSetPage w page → return $ ResInpState $ inpSt { isWin  = w
                                                      , isPage = page }
  InpActTest → do
    atomically $ writeQueue (envEventQ env) $ EventLog LogInfo $ show inpSt
    return ResInpSuccess
  InpActNULL             → return ResInpSuccess
