{-# LANGUAGE Strict #-}
-- | mouse holds a lot of function
module Vulk.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Data ( Color(..) )
import Prog ( Prog )
import Load.Data ( Tile(..) )
import qualified Vulk.GLFW as GLFW
      
-- | TODO: un-hardcode the pixels here
convertPixels ∷ ∀ a. (Fractional a, Num a) ⇒ (a,a) → (a,a)
convertPixels (x,y) = (x',y')
  where x' = x - (1280.0 / 2.0) / 64.0
        y' = - ((y - ( 720.0 / 2.0)) / 64.0)

-- | TODO: make the scroll do something
evalScroll ∷ GLFW.Window → Double → Double → Prog ε σ ()
evalScroll _ _ _ = do
--evalScroll _ _ y = do
--  env ← ask
  --liftIO . atomically $ modifyTVar' (envCamVar env) $
  -- \(Camera (cx,cy,cz) mov) → Camera
  -- (cx,cy,(min -0.1 $ max -10 $ cz - (0.1*(realToFrac y)))) mov
  return ()

-- | TODO: make the mouse do something
evalMouse ∷ GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → Prog ε σ ()
--evalMouse win mb mbs mk = return ()
evalMouse _   _  _   _  = return ()

-- | moves the camera when using the mouse
moveCamWithMouse ∷ (Double,Double) → Prog ε σ ()
moveCamWithMouse _ = return ()

-- | provides the dynamic data for a link
calcLink ∷ (Int,Int) → (Double,Double) → (Double,Double) → [Tile]
calcLink size pos _
  = [GTile pos' (1.5,0.3) (4,2) (32,32) 107 (Color 255 255 255 255)]
  where pos' = (fst pos - fst sizeNorm + 0.75
               ,(-(snd pos)) + snd sizeNorm + 0.05)
        sizeNorm = (fromIntegral (fst size)/128.0
                   ,fromIntegral (snd size)/128.0)
