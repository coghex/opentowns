-- | data structures for the lua interpreter
module Luau.Data where
-- data for lua interpreter
import Data.Map (Map)
import Elem.Data ( WinElem(..) )

-- | abstract window
data Window = Window { winTitle  ∷ String
                     , winSize   ∷ (Int,Int)
                     , winPages  ∷ [Page]
                     , winLast   ∷ String
                     } deriving (Show, Eq)

-- | each window contains pages, each page contains winElems
data Page = Page { pageTitle  ∷ String
                 , pageElems  ∷ [WinElem]
                 } deriving (Show, Eq)

-- | possible keys, so we dont have to use the GLFW one everywhere
data Key = KeyA
         | KeyB
         | KeyC
         | KeyD
         | KeyE
         | KeyF
         | KeyG
         | KeyH
         | KeyI
         | KeyJ
         | KeyK
         | KeyL
         | KeyM
         | KeyN
         | KeyO
         | KeyP
         | KeyQ
         | KeyR
         | KeyS
         | KeyT
         | KeyU
         | KeyV
         | KeyW
         | KeyX
         | KeyY
         | KeyZ
         | KeyUp
         | KeyLeft
         | KeyDown
         | KeyRight
         | KeyTilde
         | Key1
         | Key2
         | Key3
         | Key4
         | Key5
         | Key6
         | Key7
         | Key8
         | Key9
         | Key0
         | KeyMinus
         | KeyEqual
         | KeyBackspace
         | KeyTab
         | KeyBrackLeft
         | KeyBrackRight
         | KeyPipe
         | KeyCaps
         | KeyColon
         | KeyQuote
         | KeyReturn
         | KeyComma
         | KeyPeriod
         | KeySlash
         | KeySpace
         | KeyEscape
         | KeyNum0
         | KeyNum1
         | KeyNum2
         | KeyNum3
         | KeyNum4
         | KeyNum5
         | KeyNum6
         | KeyNum7
         | KeyNum8
         | KeyNum9
         | KeyNumDot
         | KeyNumDiv
         | KeyNumMul
         | KeyNumSub
         | KeyNumAdd
         | KeyNumRet
         | KeyNumEql
         | KeyF1
         | KeyF2
         | KeyF3
         | KeyF4
         | KeyF5
         | KeyF6
         | KeyF7
         | KeyF8
         | KeyF9
         | KeyF10
         | KeyF11
         | KeyF12
         | KeyF13
         | KeyF14
         | KeyF15
         | KeyF16
         | KeyF17
         | KeyF18
         | KeyF19
         | KeyF20
         | KeyF21
         | KeyF22
         | KeyF23
         | KeyF24
         | KeyF25
         | KeyLShift
         | KeyLCtrl
         | KeyLAlt
         | KeyLSuper
         | KeyRShift
         | KeyRCtrl
         | KeyRAlt
         | KeyRSuper
         | KeyInsert
         | KeyDelete
         | KeyPageUp
         | KeyPageDown
         | KeyHome
         | KeyEnd
         | KeyScrLk
         | KeyNumLk
         | KeyPrtSc
         | KeyPause
         | KeyMenu
         | KeyNULL deriving (Show, Eq, Ord)

-- | a mapping from key functionality to none or more
--   physical keys.  used in the child threads so can be lazy.
--   hlint suggests newtype here
newtype KeyMap = KeyMap (Map KeyFunc [Key]) deriving (Show,Eq,Ord)

-- | ADTs for the key layouts each KeyFunc
--   corresponds to none or many physical Keys
--   some of these are engine related, some
--   are from the original game.
data KeyFunc = KFUp
             | KFDown
             | KFLeft
             | KFRight
             | KFLvlUp
             | KFLvlDown
             | KFScrollUp
             | KFScrollDown
             | KFScrollLeft
             | KFScrollRight
             | KFEscape
             | KFShell
             | KFTest
             | KFNULL deriving (Show, Eq, Ord)
