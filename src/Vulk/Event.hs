{-# LANGUAGE Strict #-}
-- | this is all just a placeholder right now,
--   i have code tucked away somewhere
module Vulk.Event where
-- key input is handled
import Prelude()
import UPrelude
import Data ( Cardinal (..) )
import Prog ( Prog )

-- mouse bools move cam acceleration each frame
moveCamWithKeys ∷ Prog ε σ ()
moveCamWithKeys = return ()

calcCam ∷ (Double,Double) → (Double,Double,Double)
  → (Double,Double,Double)
calcCam (x,y) (cx,cy,cz) = (cx+x,cy+y,cz)

-- accelerate the inputstate
accelIS ∷ Cardinal → (Double,Double) → (Double,Double)
accelIS North (x,y) = (x, 1.1*(y - 0.1))
accelIS West  (x,y) = (1.1*(x + 0.1), y)
accelIS South (x,y) = (x, 1.1*(y + 0.1))
accelIS East  (x,y) = (1.1*(x - 0.1), y)
accelIS NorthWest (x,y) = (1.1*(x + 0.1), 1.1*(y - 0.1))
accelIS NorthEast (x,y) = (1.1*(x - 0.1), 1.1*(y - 0.1))
accelIS SouthWest (x,y) = (1.1*(x + 0.1), 1.1*(y + 0.1))
accelIS SouthEast (x,y) = (1.1*(x - 0.1), 1.1*(y + 0.1))
accelIS CardNULL (x,y) = (x,y)

decell ∷ (Double,Double) → (Double,Double)
decell (x,y)
  | (abs x < 0.01) ∧ (abs y < 0.01) = (0.0,0.0)
  | abs x < 0.01                    = (0.0,y / 1.1)
  | abs y < 0.01                    = (x / 1.1,0.0)
  | otherwise                       = (x / 1.1,y / 1.1)
