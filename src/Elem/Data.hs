-- | winelem data structures, highest non-lua level of abstract
module Elem.Data where
-- high level structures for different types of UI elements
import Prelude()
import UPrelude
import Data ( Color(..), Difficulty(..), Key(..)
            , KeyFunc(..), MapType(..), MapTiles(..) )
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
  | WinElemMap MapType MapTiles 
  | WinElemNULL deriving (Show, Eq)

-- possible button actions and defaults
data ButtAction = ButtActionKey Int KeyFunc [Key]
                | ButtActionLink String
                | ButtActionLoad String String
                | ButtActionBack
                | ButtActionExit
                | ButtActionFunc LuaFunc
                | ButtActionText TextButton
                | ButtActionNULL deriving (Show, Eq)

-- | commands accessable to lua, can be mapped onto buttons
data LuaFunc = LuaFuncToggleFullScreen
             | LuaFuncUnknown String
             | LuaFuncNULL deriving (Show, Eq)

-- | types of text buttons, used in setting state variables
data TextButton = TextMusic          Bool
                | TextMusicVolume    Int
                | TextFX             Bool
                | TextFXVolume       Int
                | TextMouseScroll    Bool
                | TextScrollHover    Bool
                | TextHeightCubes    Bool
                | TextItemDisableDef Bool
                | TextPauseOnStart   Bool
                | TextAutosave       (Maybe Int)
                | TextSieges         Difficulty
                | TextPauseOnSiege   Bool
                | TextPauseOnCaravan Bool
                | TextAllowBury      Bool
                | TextCPULevel       Int
                | TextKeyMap         (KeyFunc,[Key])
                | TextUnknown        String
                | TextNULL deriving (Show, Eq)

-- buttons are like links but change their value when clicked
data Button = Button { bFunc ∷ ButtFunc
                     , bPos  ∷ (Double,Double)
                     , bSize ∷ (Double,Double)
                     , bWin  ∷ String
                     , bPage ∷ String
                     } deriving (Show, Eq)
data ButtFunc = ButtFuncKey   Int
              | ButtFuncLink  Int
              | ButtFuncFunc  Int
              | ButtFuncText  Int
              | ButtFuncLoad  Int
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
              | InpActSetPage String String
              | InpActTest
              | InpActNULL deriving (Show, Eq)

-- | various situations in which a key may be captured
data CapType = CapKeyChange Int KeyFunc
             | CapNULL deriving (Show, Eq)


