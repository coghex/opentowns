-- | lua commands that are registered in Luau.hs are defined here.
module Luau.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Data ( Color(..), Difficulty(..), Key(..)
            , KeyFunc(..), MapType(..), MapTiles(..) )
import Data.List.Split (splitOn)
import Data.Maybe ( fromMaybe )
import Numeric ( readHex )
import Text.Read ( readMaybe )
import Elem.Data ( WinElem(..), ButtAction(..)
                 , LuaFunc(..), TextButton(..) )
import Load.Data ( LoadCmd(..) )
import Prog.Data ( Env(..) )
import Sign.Data
    ( Event(EventSys, EventLog), LogLevel(..),
      SysAction(SysReload, SysExit, SysRecreate) )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically, readTVar )
import Luau.Data ( Window(..), Page(..) )
import Luau.Util ( vtail, vhead )
import Vulk.Draw ( calcTextBoxSize )

-- | quits everything using glfw
hsExit ∷ Env → Lua.Lua ()
hsExit env = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventSys SysExit

-- | logs at level n, 1 being -v, 3 being -vvv,
--   0 being no verbosity whatsoever
hsLogDebug ∷ Env → Int → String → Lua.Lua ()
hsLogDebug env n str = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventLog (LogDebug n) str

-- | logs info, should not be used in production code
hsLogInfo ∷ Env → String → Lua.Lua ()
hsLogInfo env str = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventLog LogInfo str

-- | logs a string and ends the entire process and children
hsLogError ∷ Env → String → Lua.Lua ()
hsLogError env str = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventLog LogError str

-- | recreates the swapbuffer
hsRecreate ∷ Env → Lua.Lua ()
hsRecreate env = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventSys SysRecreate

-- | recalculates and loads dynamic data into
--   the vulkan transition functions
hsReload ∷ Env → Lua.Lua ()
hsReload env = Lua.liftIO $ atomically
  $ writeQueue (envEventQ env) $ EventSys SysReload

-- | adds a new window to the draw state
hsNewWindow ∷ Env → String → Lua.Lua()
hsNewWindow env name = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewWin win
  -- TODO: unhardcode the window size
  where win = Window name (1280,720) [] "NULL" "NULL"

-- | add a new page to the draw state
hsNewPage ∷ Env → String → String → Lua.Lua()
hsNewPage env name pname = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewPage name page
  where page = Page pname []

-- | add a new bit to the page
hsNewElem ∷ Env → String → String → String → Lua.Lua()
hsNewElem env name pname el = case head $ splitOn ":" el of
  "text" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    tailtailargs ← vtail tailargs
    tttargs      ← vtail tailtailargs
    let loadQ  = envLoadQ env
        e      = WinElemText pos color text
        text   = head args
        x'     = readMaybe (head tailargs)     ∷ Maybe Double
        y'     = readMaybe (head tailtailargs) ∷ Maybe Double
        pos    = sanitizeXY x' y'
        color  = sanitizeColor $ head  tttargs
        -- TODO: get arguments going
     --   ellink = last args
    Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewElem name pname e
  "butt" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    tailtailargs ← vtail tailargs
    tttargs      ← vtail tailtailargs
    let loadQ  = envLoadQ env
        text   = head args
        x'     = readMaybe (head tailargs)     ∷ Maybe Double
        y'     = readMaybe (head tailtailargs) ∷ Maybe Double
        pos    = sanitizeXY x' y'
        color  = sanitizeColor $ head tttargs
        -- TODO: get arguments going
        ellink = last args
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize text ttfdat
        e      = WinElemButt pos color (w,h) w (ButtActionLink ellink) (-1) text False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "load" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    tailtailargs ← vtail tailargs
    tttargs      ← vtail tailtailargs
    ttttargs     ← vtail tttargs
    let loadQ  = envLoadQ env
        text   = head args
        x'     = readMaybe (head tailargs)     ∷ Maybe Double
        y'     = readMaybe (head tailtailargs) ∷ Maybe Double
        pos    = sanitizeXY x' y'
        color  = sanitizeColor $ head tttargs
        win    = head ttttargs
        argv   = last args
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize text ttfdat
        e      = WinElemButt pos color (w,h) w (ButtActionLoad win argv) (-1) text False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "back" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    let loadQ = envLoadQ env
        x'    = readMaybe (head args)     ∷ Maybe Double
        y'    = readMaybe (head tailargs) ∷ Maybe Double
        pos   = sanitizeXY x' y'
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize "Back" ttfdat
        e      = WinElemButt pos (sanitizeColor "0xFFFFFF") (w,h) w ButtActionBack (-1) "Back" False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "exit" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    let loadQ = envLoadQ env
        x'    = readMaybe (head args)     ∷ Maybe Double
        y'    = readMaybe (head tailargs) ∷ Maybe Double
        pos   = sanitizeXY x' y'
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize "Exit Game" ttfdat
        e      = WinElemButt pos (sanitizeColor "0xFFFFFF") (w,h) w ButtActionExit (-1) "Exit Game" False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "func" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    tailtailargs ← vtail tailargs
    tttargs      ← vtail tailtailargs
    let loadQ = envLoadQ env
        x'    = readMaybe (head args)     ∷ Maybe Double
        y'    = readMaybe (head tailargs) ∷ Maybe Double
        text  = head tailtailargs
        color = sanitizeColor $ head tttargs
        func  = last args
        pos   = sanitizeXY x' y'
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize text ttfdat
        e      = WinElemButt pos color (w,h) w (ButtActionFunc (findLuaFunc func)) (-1) text False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "tBut" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    tailtailargs ← vtail tailargs
    tttargs      ← vtail tailtailargs
    let loadQ = envLoadQ env
        x'    = readMaybe (head args)     ∷ Maybe Double
        y'    = readMaybe (head tailargs) ∷ Maybe Double
        text  = head tailtailargs
        color = sanitizeColor $ head tttargs
        pos   = sanitizeXY x' y'
        def   = last args
        val   = textButton text def
        text' = sanitizeText text
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize (text' ⧺ valString val) ttfdat
        e      = WinElemButt pos color (w,h) w (ButtActionText val) (-1) text' False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "keys" → do
    args         ← vtail $ splitOn ":" el
    tailargs     ← vtail args
    tailtailargs ← vtail tailargs
    tttargs      ← vtail tailtailargs
    let loadQ = envLoadQ env
        x'    = readMaybe (head args)     ∷ Maybe Double
        y'    = readMaybe (head tailargs) ∷ Maybe Double
        text  = head tailtailargs
        color = sanitizeColor $ head tttargs
        pos   = sanitizeXY x' y'
        def   = last args
        val   = keysButton text def
        text' = sanitizeText text
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize (text' ⧺ valString val) ttfdat
        e      = WinElemButt pos color (w,h) w
                 (ButtActionKey 1 kf keys) (-1) text'' False
        keys   = sanitizeKeys def
        kf     = sanitizeKeyFunc text
        text'' = text' ⧺ " (Key: " ⧺ def ⧺ ")"
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "worldMap" → do
    Lua.liftIO $ atomically $ writeQueue (envLoadQ env)
      $ LoadCmdNewElem name pname e
        where e     = WinElemMap mtype tiles
              mtype = parseMapType $ last $ splitOn ":" el
              tiles = MapTiles (0,0) [[]]
  unk → Lua.liftIO $ atomically $ writeQueue (envEventQ env)
    $ EventLog LogWarn $ "unknown element: " ⧺ unk

-- | sanitize map type
parseMapType ∷ String → MapType
parseMapType "normalmap"    = MapNormal
parseMapType "desertmap"    = MapDesert
parseMapType "junglemap"    = MapJungle
parseMapType "mixedmap"     = MapMixed
parseMapType "snowmap"      = MapSnow
parseMapType "mountainsmap" = MapMountains
parseMapType _              = MapNULL

-- | sanitize default key function
sanitizeKeyFunc ∷ String → KeyFunc
sanitizeKeyFunc "Scroll up"     = KFScrollUp
sanitizeKeyFunc "Scroll down"   = KFScrollDown
sanitizeKeyFunc "Scroll left"   = KFScrollLeft
sanitizeKeyFunc "Scroll right"  = KFScrollRight
sanitizeKeyFunc "Level up"      = KFLvlUp
sanitizeKeyFunc "Level down"    = KFLvlDown
sanitizeKeyFunc unk         = KFUnknown unk
-- | reverse operation
unsanitizeKeyFunc ∷ KeyFunc → String
unsanitizeKeyFunc KFScrollUp      = "Scroll up"
unsanitizeKeyFunc KFScrollDown    = "Scroll down"
unsanitizeKeyFunc KFScrollLeft    = "Scroll left"
unsanitizeKeyFunc KFScrollRight   = "Scroll right"
unsanitizeKeyFunc KFLvlUp         = "Level up"
unsanitizeKeyFunc KFLvlDown       = "Level down"
unsanitizeKeyFunc (KFUnknown unk) = unk
unsanitizeKeyFunc KFNULL          = "NULL"

-- | sanitizes default keys
sanitizeKeys ∷ String → [Key]
sanitizeKeys str = map sanitizeKey list
  where list = splitOn "," str
-- | reverse operation
unsanitizeKeys ∷ [Key] → String
unsanitizeKeys ks = " (Key: " ⧺ unsanitizeKeysF ks
unsanitizeKeysF ∷ [Key] → String
unsanitizeKeysF []     = "()"
unsanitizeKeysF [k]    = unsanitizeKey k ⧺ ")"
unsanitizeKeysF (k:ks) = unsanitizeKey k ⧺ "," ⧺ unsanitizeKeysF ks

-- | turns lua string reference into ADT with default value, if
--   the user supplied default is nonsense, we use our own
textButton ∷ String → String → TextButton
textButton "Music"        def = TextMusic       $ sanitizeBool True def
textButton "Music-Volume" def = TextMusicVolume $ sanitize100  100  def
textButton "FX"           def = TextFX          $ sanitizeBool True def
textButton "FX-Volume"    def = TextFXVolume    $ sanitize100  100  def
textButton "Mouse scroll" def = TextMouseScroll $ sanitizeBool True def
textButton "Allow mouse scroll while hovering the edge buttons" def
  = TextScrollHover $ sanitizeBool False def
textButton "Height cubes when 2D mouse is enabled" def
  = TextHeightCubes $ sanitizeBool True def
textButton "Newly built stockpiles/containers have all items disabled by default" def
  = TextItemDisableDef $ sanitizeBool False def
textButton "Pause the game when it starts" def
  = TextPauseOnStart $ sanitizeBool False def
textButton "Autosave" def
  = TextAutosave $ sanitizeDays Nothing def 
textButton "Sieges" def
  = TextSieges $ sanitizeDifficulty DNormal def
textButton "Pause the game when a siege starts" def
  = TextPauseOnSiege $ sanitizeBool False def
textButton "Pause the game when a caravan comes" def
  = TextPauseOnCaravan $ sanitizeBool False def
textButton "Allow bury system" def
  = TextAllowBury $ sanitizeBool True def
textButton "CPU level usage for pathfinding" def
  = TextCPULevel $ sanitizeCPU 2 def
textButton str            def = TextUnknown     $ str ⧺ ": " ⧺ def
-- | finds the string of the default text button,
--   just for measurement of the predefined width.
--   these are the longest strings in possible values
valString ∷ TextButton → String
valString (TextMusic          _) = "OFF"
valString (TextMusicVolume    _) = "100%"
valString (TextFX             _) = "OFF"
valString (TextFXVolume       _) = "100%"
valString (TextMouseScroll    _) = "OFF"
valString (TextScrollHover    _) = "OFF"
valString (TextHeightCubes    _) = "OFF"
valString (TextItemDisableDef _) = "OFF"
valString (TextPauseOnStart   _) = "OFF"
valString (TextAutosave       _) = "Disabled"
valString (TextSieges         _) = "Disabled"
valString (TextPauseOnSiege   _) = "OFF"
valString (TextPauseOnCaravan _) = "OFF"
valString (TextAllowBury      _) = "OFF"
valString (TextCPULevel       _) = "0"
valString (TextKeyMap         _) = "Key: KeyFunc: "
valString (TextUnknown        _) = "UNK: "
valString  TextNULL              = "NULL"
-- | keys have a similar behavior
keysButton ∷ String → String → TextButton
keysButton str def = TextKeyMap (kf,keys)
  where kf   = sanitizeKeyFunc str
        keys = sanitizeKeys    def

-- | uses the default values if they sanitize
sanitizeBool ∷ Bool → String → Bool
sanitizeBool _   "ON"  = True
sanitizeBool _   "OFF" = False
sanitizeBool def _     = def
sanitize100 ∷ Int → String → Int
sanitize100 def str = fromMaybe def $ readMaybe str
sanitizeDays ∷ Maybe Int → String → Maybe Int
sanitizeDays _   "Disabled" = Nothing
sanitizeDays def str        = maybe def Just (readMaybe [head str])
--sanitizeDays def str        = case (readMaybe [head str]) of
--  Nothing → def
--  Just n0 → Just n0
sanitizeDifficulty ∷ Difficulty → String → Difficulty
sanitizeDifficulty _   "Normal" = DNormal
sanitizeDifficulty def _        = def
-- | quicker to just enumerate
sanitizeCPU ∷ Int → String → Int
sanitizeCPU _   "1" = 1
sanitizeCPU _   "2" = 2
sanitizeCPU _   "3" = 3
sanitizeCPU _   "4" = 4
sanitizeCPU _   "5" = 5
sanitizeCPU _   "6" = 6
sanitizeCPU def _   = def
--sanitizeKeys ∷ (KeyFunc,[Key]) → String → (KeyFunc,[Key])
--sanitizeKeys (kf,keys) str = (kf,sanitizeKeyStr str keys)
--sanitizeKeyStr ∷ String → [Key] → [Key]
--sanitizeKeyStr str keys = case splitOn "," str of
--  [a,b] → [sanitizeKey a,sanitizeKey b]
--  _     → keys
sanitizeKey ∷ String → Key
sanitizeKey "A"      = KeyA
sanitizeKey "B"      = KeyB
sanitizeKey "C"      = KeyC
sanitizeKey "D"      = KeyD
sanitizeKey "E"      = KeyE
sanitizeKey "F"      = KeyF
sanitizeKey "G"      = KeyG
sanitizeKey "H"      = KeyH
sanitizeKey "I"      = KeyI
sanitizeKey "J"      = KeyJ
sanitizeKey "K"      = KeyK
sanitizeKey "L"      = KeyL
sanitizeKey "M"      = KeyM
sanitizeKey "N"      = KeyN
sanitizeKey "O"      = KeyO
sanitizeKey "P"      = KeyP
sanitizeKey "Q"      = KeyQ
sanitizeKey "R"      = KeyR
sanitizeKey "S"      = KeyS
sanitizeKey "T"      = KeyT
sanitizeKey "U"      = KeyU
sanitizeKey "V"      = KeyV
sanitizeKey "W"      = KeyW
sanitizeKey "X"      = KeyX
sanitizeKey "Y"      = KeyY
sanitizeKey "Z"      = KeyZ
sanitizeKey "Up"     = KeyUp
sanitizeKey "Down"   = KeyDown
sanitizeKey "Right"  = KeyRight
sanitizeKey "Left"   = KeyLeft
sanitizeKey "<"      = KeyComma
sanitizeKey ">"      = KeyPeriod
sanitizeKey unk = KeyUnknown unk
unsanitizeKey ∷ Key → String
unsanitizeKey KeyA      = "A"      
unsanitizeKey KeyB      = "B"      
unsanitizeKey KeyC      = "C"      
unsanitizeKey KeyD      = "D"      
unsanitizeKey KeyE      = "E"      
unsanitizeKey KeyF      = "F"      
unsanitizeKey KeyG      = "G"      
unsanitizeKey KeyH      = "H"      
unsanitizeKey KeyI      = "I"      
unsanitizeKey KeyJ      = "J"      
unsanitizeKey KeyK      = "K"      
unsanitizeKey KeyL      = "L"      
unsanitizeKey KeyM      = "M"      
unsanitizeKey KeyN      = "N"      
unsanitizeKey KeyO      = "O"      
unsanitizeKey KeyP      = "P"      
unsanitizeKey KeyQ      = "Q"      
unsanitizeKey KeyR      = "R"      
unsanitizeKey KeyS      = "S"      
unsanitizeKey KeyT      = "T"      
unsanitizeKey KeyU      = "U"      
unsanitizeKey KeyV      = "V"      
unsanitizeKey KeyW      = "W"      
unsanitizeKey KeyX      = "X"      
unsanitizeKey KeyY      = "Y"      
unsanitizeKey KeyZ      = "Z"      
unsanitizeKey KeyUp     = "Up"
unsanitizeKey KeyDown   = "Down"
unsanitizeKey KeyRight  = "Right"
unsanitizeKey KeyLeft   = "Left"
unsanitizeKey KeyComma  = "<"
unsanitizeKey KeyPeriod = ">"
unsanitizeKey (KeyUnknown unk) = unk


-- | turns lua string reference into lua function ADT
findLuaFunc ∷ String → LuaFunc
findLuaFunc "toggleFullScreen" = LuaFuncToggleFullScreen
findLuaFunc str                = LuaFuncUnknown str

-- | some text buttons are identical, so we added hyphens,
--   here we get rid of said hyphens
sanitizeText ∷ String → String
sanitizeText "Music-Volume" = "Volume"
sanitizeText "FX-Volume"    = "Volume"
sanitizeText str            = str

-- | makes sure x,y pair strings are readable, if not, returns 0's
sanitizeXY ∷ Maybe Double → Maybe Double → (Double,Double)
sanitizeXY Nothing  Nothing  = (0,0)
sanitizeXY (Just x) Nothing  = (x,0)
sanitizeXY Nothing  (Just y) = (0,y)
sanitizeXY (Just x) (Just y) = (x,y)
-- | makes sure the hex color values are legible, if not, returns
--   (1,1,1,0). accepts formats "0xRRGGBB" "RRGGBB", with optional "AA"
sanitizeColor ∷ String → Color
sanitizeColor ""  = Color 255 255 255 255
sanitizeColor [_] = Color 255 255 255 255
sanitizeColor str = if (head str ≡ '0') ∧ (head (tail str) ≡ 'x') then
  sanitizeColorF (tail (tail str)) else sanitizeColorF str
-- | sanitizes hex values after "0x" is stripped
sanitizeColorF ∷ String → Color
sanitizeColorF str
  | length str ≡ 6  = Color r   g   b   255
  | length str ≡ 8  = Color r   g   b   a
  | otherwise       = Color 255 255 255 255
  where (r,_) = head $ readHex r'
        (g,_) = head $ readHex g'
        (b,_) = head $ readHex b'
        (a,_) = head $ readHex a'
        r'    = [str ‼ 0,str ‼ 1]
        g'    = [str ‼ 2,str ‼ 3]
        b'    = [str ‼ 4,str ‼ 5]
        a'    = [str ‼ 6,str ‼ 7]
-- | switches to page by name
hsGoToPage ∷ Env → String → Lua.Lua()
hsGoToPage env name = case splitOn ":" name of
  [n1,n2] → do
    let loadQ = envLoadQ env
    Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdSwitchWin n1 n2
  _ → do
    Lua.liftIO $ atomically $ writeQueue (envEventQ env) $ EventLog LogError "go to page requires win and page"
