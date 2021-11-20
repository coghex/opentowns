-- | lua commands that are registered in Luau.hs are defined here.
module Luau.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified Foreign.Lua as Lua
import Data ( Color(..) )
import Data.List.Split (splitOn)
import Data.Maybe ( fromMaybe )
import Numeric ( readHex )
import Text.Read ( readMaybe )
import Elem.Data ( WinElem(..), ButtAction(..), LuaFunc(..) )
import Load.Data ( LoadCmd(..) )
import Prog.Data ( Env(..) )
import Sign.Data
    ( Event(EventSys, EventLog), LogLevel(..),
      SysAction(SysReload, SysExit, SysRecreate) )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically, readTVar )
import Luau.Data ( Window(..), Page(..) )
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
    let loadQ  = envLoadQ env
        e      = WinElemText pos color text
        args   = tail $ splitOn ":" el
        text   = head args
        x'     = readMaybe (head (tail args))        ∷ Maybe Double
        y'     = readMaybe (head (tail (tail args))) ∷ Maybe Double
        pos    = sanitizeXY x' y'
        color  = sanitizeColor $ head $ tail $ tail $ tail args
        -- TODO: get arguments going
     --   ellink = last args
    Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdNewElem name pname e
  "butt" → do
    let loadQ  = envLoadQ env
        args   = tail $ splitOn ":" el
        text   = head args
        x'     = readMaybe (head (tail args))        ∷ Maybe Double
        y'     = readMaybe (head (tail (tail args))) ∷ Maybe Double
        pos    = sanitizeXY x' y'
        color  = sanitizeColor $ head $ tail $ tail $ tail args
        -- TODO: get arguments going
        ellink = last args
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize text ttfdat
        e      = WinElemButt pos color (w,h) w (ButtActionLink ellink) (-1) text False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "back" → do
    let loadQ = envLoadQ env
        args  = tail $ splitOn ":" el
        x'    = readMaybe (head args)        ∷ Maybe Double
        y'    = readMaybe (head (tail args)) ∷ Maybe Double
        pos   = sanitizeXY x' y'
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize "Back" ttfdat
        e      = WinElemButt pos (sanitizeColor "0xFFFFFF") (w,h) w ButtActionBack (-1) "Back" False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "exit" → do
    let loadQ = envLoadQ env
        args  = tail $ splitOn ":" el
        x'    = readMaybe (head args)        ∷ Maybe Double
        y'    = readMaybe (head (tail args)) ∷ Maybe Double
        pos   = sanitizeXY x' y'
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize "Exit Game" ttfdat
        e      = WinElemButt pos (sanitizeColor "0xFFFFFF") (w,h) w ButtActionExit (-1) "Exit Game" False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "func" → do
    let loadQ = envLoadQ env
        args  = tail $ splitOn ":" el
        x'    = readMaybe (head args)        ∷ Maybe Double
        y'    = readMaybe (head (tail args)) ∷ Maybe Double
        text  = head (tail (tail args))
        color = sanitizeColor $ head (tail (tail (tail args)))
        func  = last args
        pos   = sanitizeXY x' y'
    ttfdat' ← Lua.liftIO $ atomically $ readTVar (envFontM env)
    let ttfdat = fromMaybe [] ttfdat'
        (w,h)  = calcTextBoxSize text ttfdat
        e      = WinElemButt pos color (w,h) w (ButtActionFunc (findLuaFunc func)) (-1) text False
    Lua.liftIO $ atomically $ writeQueue loadQ
      $ LoadCmdNewElem name pname e
  "tBut" → return ()
  unk → Lua.liftIO $ atomically $ writeQueue (envEventQ env)
    $ EventLog LogWarn $ "unknown element: " ⧺ unk

-- | turns lua string reference into lua function ADT
findLuaFunc ∷ String → LuaFunc
findLuaFunc "toggleFullScreen" = LuaFuncToggleFullScreen
findLuaFunc str                = LuaFuncUnknown str

-- | makes sure x,y pair strings are readable, if not, returns 0's
sanitizeXY ∷ Maybe Double → Maybe Double → (Double,Double)
sanitizeXY Nothing  Nothing  = (0,0)
sanitizeXY (Just x) Nothing  = (x,0)
sanitizeXY Nothing  (Just y) = (0,y)
sanitizeXY (Just x) (Just y) = (x,y)
-- | makes sure the hex color values are legible, if not, returns
-- | (1,1,1,0). accepts formats "0xRRGGBB" "RRGGBB", with optional "AA"
sanitizeColor ∷ String → Color
sanitizeColor str = if (head str ≡ '0') ∧ (head (tail str) ≡ 'x') then
  sanitizeColorF (tail (tail str)) else sanitizeColorF str
-- | sanitizes hex values after "0x" is stripped
sanitizeColorF ∷ String → Color
sanitizeColorF str
  | length str ≡ 6  = Color r g b 1
  | length str ≡ 8  = Color r g b a
  | otherwise       = Color 1 1 1 1
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
hsGoToPage env name = do
  let loadQ = envLoadQ env
  Lua.liftIO $ atomically $ writeQueue loadQ $ LoadCmdSwitchWin name
