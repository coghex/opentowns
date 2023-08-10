{-# LANGUAGE Strict #-}
-- | all input events are, in their callbacks, sending this thread
--   the input in the input queue.  it is then processed here
module Prog.Input where
-- a thread to handle input
import Prelude()
import UPrelude
import Data ( KeyFunc(..), KeyMap(..), PopupType(..), Cardinal(..) )
import Data.Maybe ( fromMaybe )
import Elem.Data
    ( CapType(..),
      Button(..), ButtFunc(..),
      InputAct(..), InputElem(..) )
import Load.Data ( LoadCmd(..), DrawStateCmd(..) )
import Prog.Data
    ( Env(..), ISKeys(..),
      ISStatus(ISSNULL, ISSLogDebug),
      InpResult(..),
      InputState(..) )
import Prog.KeyEvent ( changeKeyMap, findKey, lookupKey, indexKeyMap
                     , stateKeyPress, stateKeyRelease )
import Prog.Init ( initKeyMap, initInpState )
import Prog.Mouse ( processLoadMouse, findAllButtsUnder, findButts )
import Prog.Util ()
import Sign.Data
    ( Event(..), LogLevel(..),
      SysAction(..), TState(..)
    , SettingsChange(..), InputStateChange(..) )
import Sign.Var ( atomically, readTVar, writeTVar )
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
  processInputSideEffects env inpSt2
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
--          atomically $ writeQueue (envEventQ env)
--            $ EventLog (LogDebug 1) $ "new keys: " ⧺ show keymap'
          atomically $ writeQueue (envLoadQ env)
            $ LoadCmdDS $ DSCUpdatePopup $ PopupSetKey 2 keyFunc [secondkey]
          processLoadInputs env win inpSt' keymap'
            where keymap' = changeKeyMap keyFunc key 1 keymap
                  inpSt'  = inpSt { inpCap = CapKeyChange 2 keyFunc }
                  secondkey = last $ indexKeyMap keymap' keyFunc
        ResInpChangeKey keyFunc key 2 → do
--          atomically $ writeQueue (envEventQ env)
--            $ EventLog (LogDebug 1) $ "new keys: " ⧺ show keymap'
          atomically $ writeQueue (envEventQ env)
            $ EventSettings $ SettingsChangeKeyMap keymap'
          atomically $ writeQueue (envLoadQ env)
            $ LoadCmdDS $ DSCClearPopup $ PopupSetKey 1 keyFunc []
          atomically $ writeQueue (envLoadQ env)
            $ LoadCmdDS $ DSCUpdateKeyButton keyFunc $ indexKeyMap keymap' keyFunc
          processLoadInputs env win inpSt' keymap'
            where keymap' = changeKeyMap keyFunc key 2 keymap
                  inpSt'  = inpSt { inpCap = CapNULL }
        ResInpChangeKey keyFunc key n → do
        -- code should never reach here
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
  -- window, we need to function differently
  InpActKey k ks _  → case inpCap inpSt of
    -- input captured by key change pop up window
    CapKeyChange n keyFunc → if ks ≡ GLFW.KeyState'Pressed then do
        --print $ "captured: " ⧺ show k ⧺ " for key func: " ⧺ show keyFunc
        -- test keys still work for now
        if lookupKey keymap (findKey k) ≡ KFTest
          then do
            atomically $ writeQueue (envLoadQ env) LoadCmdTest
            return ResInpSuccess
        else if lookupKey keymap (findKey k) ≡ KFTest2
          then do
            atomically $ writeQueue (envLoadQ env) LoadCmdTest2
            return ResInpSuccess
        else return $ ResInpChangeKey keyFunc (findKey k) n
      else return ResInpSuccess
    -- for when text is being input
    CapTextInput str → if ks ≡ GLFW.KeyState'Pressed then do
        --print $ "captured: " ⧺ show k ⧺ " for text input"
        if lookupKey keymap (findKey k) ≡ KFReturn
          then do
            atomically $ writeQueue (envLoadQ env) $ LoadCmdDS $ DSCClearPopup $ PopupSavename str
            atomically $ writeQueue (envLoadQ env) $ LoadCmdDS $ DSCSavename str
            return $ ResInpState inpSt { inpCap = CapNULL }
          else do
            sc ← GLFW.getKeyScancode k
            k' ← GLFW.getKeyName k sc
            let newstr = str ⧺ fromMaybe [] k'
            atomically $ writeQueue (envLoadQ env) $ LoadCmdDS $ DSCUpdatePopup $ PopupSavename newstr
            return $ ResInpState inpSt { inpCap = CapTextInput newstr }
      else return ResInpSuccess
    -- if no input capture, case on each key function, set global inp state through event
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
        KFUnknown str → do
          atomically $ writeQueue (envEventQ env) $ EventLog LogWarn $ "key " ⧺ str ⧺ " not set in key map"
          return ResInpSuccess
        keyFunc → do
          --atomically $ writeQueue (envEventQ env)
          --  $ EventLog (LogDebug 1) $ "unknown key " ⧺ show keyFunc
          -- update global input state
          atomically $ writeQueue (envEventQ env)
            $ EventInputState $ ISCKeyPress keyFunc
          let newis = stateKeyPress keyFunc inpSt
          return $ ResInpState newis
      else if ks ≡ GLFW.KeyState'Released then case lookupKey keymap (findKey k) of
        keyFunc → do
          atomically $ writeQueue (envEventQ env)
            $ EventInputState $ ISCKeyRelease keyFunc
          let newis = stateKeyRelease keyFunc inpSt
          return $ ResInpState newis
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
          Button (ButtFuncBack _) _ _ _ _ → do
            atomically $ writeQueue (envInpQ env) $ InpActButton $ head butts
            atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ InpActButton $ head butts
            return ResInpSuccess
          Button (ButtFuncFunc _) _ _ _ _ → do
            atomically $ writeQueue (envInpQ env) $ InpActButton $ head butts
            atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ InpActButton $ head butts
            return ResInpSuccess
          Button (ButtFuncLoad _) _ _ _ _ → do
            atomically $ writeQueue (envInpQ env) $ InpActButton $ head butts
            atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ InpActButton $ head butts
            return ResInpSuccess
          Button (ButtFuncText _) _ _ _ _ → do
            atomically $ writeQueue (envInpQ env) $ InpActButton $ head butts
            atomically $ writeQueue (envLoadQ env) $ LoadCmdInput $ InpActButton $ head butts
            return ResInpSuccess

          _ → return ResInpSuccess
     --   print $ show pos
    else return ResInpSuccess
  InpActSetCap cap → do
    atomically $ writeQueue (envEventQ env) $ EventLog (LogDebug 2) $ "capture set: " ⧺ show cap
    return $ ResInpState inpSt'
    where inpSt' = inpSt { inpCap = cap }
  InpActSwitchWin w0  → return $ ResInpState inpSt'
    where inpSt' = inpSt { isWin = w0 }
  InpActSetLink butt → do
    return $ ResInpState inpSt'
    where inpSt' = inpSt { isElems = isElems inpSt ⧺ [IEButt butt] }
  -- InpActButton butt → do
    --print $ "button press: " ⧺ show butt
  InpActButton _         → return ResInpSuccess
  InpActClearPopup → return ResInpSuccess
  InpActSetPage w page → return $ ResInpState $ inpSt { isWin  = w
                                                      , isPage = page }
  InpActTest → do
    atomically $ writeQueue (envEventQ env) $ EventLog LogInfo $ show inpSt
    return ResInpSuccess
  InpActNULL             → return ResInpSuccess

-- execute side effects of input state
processInputSideEffects ∷ Env → InputState → IO ()
processInputSideEffects env is = do
  -- change camera acceleration
  camChan ← atomically $ readTVar (envCam env)
  let keyst   = keySt is
  case camChan of
      Nothing      → atomically $ writeTVar (envCam env) (Just (0,0,-1))
      Just (x,y,z) → do
        let xd      = x - (x * 0.1)
            yd      = y - (y * 0.1)
            (i,j)   = keyAccel keyst
            (i',j') = case keyst of
              ISKeys True  True  True  True  _ → (0,0)
              ISKeys False True  True  True  _ → (0,jd+1.0)
              ISKeys True  False True  True  _ → (id-1.0,0)
              ISKeys True  True  False True  _ → (0,jd-1.0)
              ISKeys True  True  True  False _ → (id+1.0,0)
              ISKeys False False True  True  _ → (id-0.5,jd+0.5)
              ISKeys False True  False True  _ → (0,jd)
              ISKeys False True  True  False _ → (id+0.5,jd+0.5)
              ISKeys True  False False True  _ → (id-0.5,jd-0.5)
              ISKeys True  False True  False _ → (id,0)
              ISKeys True  True  False False _ → (id+0.5,jd-0.5)
              ISKeys True  False False False _ → (0,jd-1.0)
              ISKeys False True  False False _ → (i+1.0,0)
              ISKeys False False True  False _ → (0,jd+1.0)
              ISKeys False False False True  _ → (i-1.0,0)
              ISKeys False False False False _ → (id,jd)
            (x',y') = (x+i',y+j')
            id      = i-(0.1*i)
            jd      = j-(0.1*j)
        atomically $ writeTVar (envCam env) (Just (x',y',z))
        atomically $ writeQueue (envEventQ env)
          $ EventInputState $ ISCAccelerate (i',j')
  return ()
