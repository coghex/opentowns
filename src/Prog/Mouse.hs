{-# LANGUAGE Strict #-}
-- | mouse related functionality, inluding scroll,
--   click, and positional monitoring 
module Prog.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Elem.Data
    ( Button(..),
      ButtFunc(..) )
import Load.Data ( LoadCmd(..), DrawStateCmd(..) )
import Prog ( MonadIO(liftIO), Prog, MonadReader(ask) )
import Prog.Data
    ( Env(..),
      InputAct(InpActMouse),
      InputState(..),
      InputElem(..) )
import Prog.Util ( logInfo )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically )
import qualified Vulk.GLFW as GLFW

-- | scroll processing
evalScroll ∷ GLFW.Window → Double → Double → Prog ε σ ()
evalScroll _ _ _ = do
--  env ← ask
  logInfo "TODO: scroll input"

-- | processes mouse on click
evalMouse ∷ GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → Prog ε σ ()
evalMouse _   mb mbs mk = do
  env ← ask
  liftIO $ atomically $ writeQueue (envInpQ env) $ InpActMouse mb mbs mk
--  when ((mb ≡ GLFW.mousebutt1)
--         ∧ (not (GLFW.modifierKeysControl mk))) $ do
--    if (mbs ≡ GLFW.MouseButtonState'Pressed) then do
--      pos   ← liftIO $ GLFW.getCursorPos win
--      env   ← ask
--      oldIS ← gets stInput
--      let loadQ = envLoadQ env
--          newIS = oldIS { mouse1 =
--            Just (realToFrac (fst pos), realToFrac (snd pos)) }
--      modify' $ \s → s { stInput = newIS }
--    else if (mbs ≡ GLFW.MouseButtonState'Released) then do
--      oldIS ← gets stInput
--      let newIS = oldIS { mouse1 = Nothing }
--      --env ← ask
--      --pos ← liftIO $ GLFW.getCursorPos win
--      --winsize ← liftIO $ GLFW.getWindowSize win
--      --liftIO $ atomically $ writeQueue (envEventQ env) $ EventLogInfo
--         $ "mouse: " ⧺ (show pos) ⧺ ", normalised: "
--         ⧺ (show (normaliseCoord (pos) (winsize)))
--      modify' $ \s → s { stInput = newIS }
--    else return ()

-- | converts pixel coords into screen coords, right now
--   screen size has no effect on how zoomed in things are
--   so we can ignore it, this may change
normaliseCoord ∷ (Double,Double) → (Int,Int) → (Double,Double)
normaliseCoord (mx,my) (_ ,_ ) = (x,y)
  where x = mx / 64.0
        y = my / 64.0

-- | processes mouse position every frame
processLoadMouse ∷ Env → GLFW.Window → InputState → IO InputState
processLoadMouse env win inpSt = do
  pos ← GLFW.getCursorPos win
--  case linkTest (findLinks (isElems inpSt)) pos of
--    Just l0 → atomically $ writeQueue (envLoadQ env) $ LoadCmdLink l0
--    Nothing → return ()
  let inpSt' = inpSt { mousePos = pos }
      -- TODO: unhardcode window size
      butts  = findAllButtsUnder w0 (1280,720) (findButts (isElems inpSt)) pos
      w0     = isWin inpSt'
  atomically $ writeQueue (envLoadQ env) $ LoadCmdDS $ DSCToggleButts butts
--      links  = findAllLinksUnder win (1280,720)
--                 (findLinks (isElems inpSt)) pos
--      butts  = findAllButtsUnder win (1280,720)
--                 (findButts (isElems inpSt)) pos
--      win    = isWin inpSt'
  --if (length links) > 0 then do
--  atomically $ writeQueue (envLoadQ env) $ LoadCmdToggleLinks links
--  atomically $ writeQueue (envLoadQ env) $ LoadCmdToggleButts butts
  return inpSt'
  --else return inpSt'

findButts ∷ [InputElem] → [Button]
findButts []                  = []
findButts ((IEButt butt):ies) = [butt] ⧺ findButts ies
findButts (_:ies)             = findButts ies

findAllButtsUnder ∷ String → (Int,Int) → [Button] → (Double,Double) → [Button]
findAllButtsUnder _   _    []     _   = []
findAllButtsUnder win size (b:bs) pos
  | buttUnder win size b pos = [b] ⧺ findAllButtsUnder win size bs pos
  | otherwise                = findAllButtsUnder win size bs pos

buttUnder ∷ String → (Int,Int) → Button → (Double,Double) → Bool
buttUnder name _ (Button _ (x,y) (w,h) win) (mx,my)
  | name ≡ win = (abs((mx / 64.0) - x - 0.5) < (0.5*w)) ∧ (abs((my / 64.0) - y) < (0.25*h))
  | otherwise  = False
