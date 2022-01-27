-- | Stacks are just a simple data structure with pop and stash
module Load.Stack where
-- a simple stack is defined
import Prelude()
import UPrelude
import Data ( Stack )

-- | classic stash/pop implemented as list
stash ∷ Stack α → α → Stack α
stash s e = e:s
pop ∷ Stack α → Maybe (α,Stack α)
pop [] = Nothing
pop s  = Just (head s, tail s)
-- | lazy pop for a string
popS ∷ Stack String → (String, Stack String)
popS [] = ("NULL",[])
popS s  = (head s, tail s)
-- | lazy pop for a string tuple
popSS ∷ Stack (String,String) → ((String,String), Stack (String,String))
popSS [] = (("NULL","NULL"),[])
popSS s  = (head s, tail s)

-- | previous element
prev ∷ Stack α → Maybe α
prev []  = Nothing
prev [_] = Nothing
prev s   = Just $ head $ tail s

-- | returns the index if elem is in the stack
elemOf ∷ (Eq α) ⇒ Stack α → α → Maybe Int
elemOf s = elemOfF (length s) s
elemOfF ∷ (Eq α) ⇒ Int → Stack α → α → Maybe Int
elemOfF _ []     _ = Nothing
elemOfF n (s:ss) e
  | s ≡ e     = Just n
  | otherwise = elemOfF (n - 1) ss e

-- | pops n elements off of the list
trim ∷ Stack α → Int → Stack α
trim s n = trimF s $ length s - n
trimF ∷ Stack α → Int → Stack α
trimF s      0 = s
trimF []     _ = []
trimF (_:ss) n = trimF ss $ n - 1

-- | changes the stack the have the given elem as the head,
--   if it doesnt exist, stashs, and returns new stack
changeStack ∷ (Eq α) ⇒ α → Stack α → Stack α
changeStack el stack = case elemOf stack el of
  Nothing → stash stack el
  Just n0 → trim stack n0
