-- | these are all either ADTs or simple structures
--   made up of ints, strings, floats, etc...
module Data where
-- the most generic structures are found
import Prelude()
import UPrelude

-- | generic 0-255 rgba structure
data Color = Color Int Int Int Int deriving (Show, Eq)

-- | cardinal directions
data Cardinal = North | South | West | East
              | NorthWest | NorthEast | SouthWest
              | SouthEast | CardNULL deriving (Show, Eq)

-- | print args logInfo specific info from the lowest levels
data PrintArg = PrintNULL deriving (Show, Eq)

-- | fps defined as actual and desired,
--   and whether or not to display
data FPS = FPS Double Int Bool deriving (Show, Eq)

-- | generic camera for movement
data Camera = Camera { cam ∷ (Double,Double,Double)
                     , mov ∷ (Double,Double) } deriving (Show, Eq)

-- | possible difficulties
data Difficulty = DNormal
                | DHard
                | DHarder
                | DInsane
                | DDisabled
                | DEasy
                | DNULL deriving (Show, Eq)

-- TODO: move this to a better place
-- | lua shell executes commands in global state
data Shell = Shell { shPrompt ∷ String
                   , shTabbed ∷ Maybe Int
                   , shCursor ∷ Int
                   , shInpStr ∷ String
                   , shCache  ∷ String
                   , shOutStr ∷ String
                   , shRet    ∷ String
                   , shLoaded ∷ Bool
                   , shHistI  ∷ Int
                   , shHist   ∷ [String] } deriving (Show, Eq)

-- | TODO: this is just a placeholder
data Popup = Popup deriving (Show,Eq)
