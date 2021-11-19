-- | winelem data structures, highest non-lua level of abstract
module Elem.Data where
-- high level structures for different types of UI elements
import Prelude()
import UPrelude
import Data ( Color(..), Difficulty(..), Key(..), KeyFunc(..) )
import qualified Vulk.GLFW as GLFW

-- |  various win elements and their associated data
data WinElem
  = WinElemText { textPos   ∷ (Double,Double) -- ^ opengl style coords
                , textColor ∷ Color -- ^ color in 0-255 rgba
                , textStr   ∷ String }
  | WinElemButt { buttPos   ∷ (Double,Double) -- ^ opengl style coords
                , textColor ∷ Color -- ^ color in 0-255 rgba
                , buttBox   ∷ (Double,Double) -- ^ the size of the box
                                              --   surrounding the butt
                , buttAdv   ∷ Double -- ^ distance from left to right
                , buttAct   ∷ ButtAction -- ^ button function
                , buttInd   ∷ Int -- ^ the index of the button in list
                                  --   of all buttons
                , buttVal   ∷ String -- ^ string displayed on button
                , buttOver  ∷ Bool } -- ^ bool of mouse over button
  | WinElemNULL deriving (Show, Eq)

-- possible button actions and defaults
data ButtAction = ButtActionMusic Bool
                | ButtActionMusicVolume Int
                | ButtActionFX Bool
                | ButtActionFXVolume Int
                | ButtActionMouseScroll Bool
                | ButtActionEdgeMouseScroll Bool
                | ButtActionHeightCube Bool
                | ButtActionItemsDisabledDefault Bool
                | ButtActionPauseGameStart Bool
                | ButtActionAutosave Int
                | ButtActionSieges Difficulty
                | ButtActionSiegePause Bool
                | ButtActionCaravanPause Bool
                | ButtActionBurySystem Bool
                | ButtActionCPUUsageLevel Int
                | ButtActionKey KeyFunc [Key]
                | ButtActionLink String
                | ButtActionNULL deriving (Show, Eq)

-- buttons are like links but change their value when clicked
data Button = Button { bFunc ∷ ButtFunc
                     , bPos  ∷ (Double,Double)
                     , bSize ∷ (Double,Double)
                     , bWin  ∷ String
                     , bPage ∷ String
                     } deriving (Show, Eq)
data ButtFunc = ButtFuncOnOff Int
              | ButtFunc100   Int
              | ButtFuncDays  Int
              | ButtFuncDiff  Int
              | ButtFuncCPU   Int
              | ButtFuncKey   Int
              | ButtFuncLink  Int
              | ButtFuncNULL deriving (Show, Eq)

-- | input state related to various winelems
data InputElem = IEButt Button | IENULL deriving (Show, Eq)
-- | possible input actions
data InputAct = InpActKey GLFW.Key GLFW.KeyState GLFW.ModifierKeys 
              | InpActMouse GLFW.MouseButton
                  GLFW.MouseButtonState GLFW.ModifierKeys
              | InpActSwitchWin String
              | InpActSetCap CapType
              | InpActSetLink Button
              | InpActButton Button
              | InpActNULL deriving (Show, Eq)

-- | various situations in which a key may be captured
data CapType = CapKeyChange Int KeyFunc
             | CapNULL deriving (Show, Eq)


