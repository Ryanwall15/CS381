module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- Ryan Wallerius ONID: wallerir
-- Daniyal Abbas  ONID: abbasd
-- Date: 08-05-2019
-- HW 4

-- | Valuation function for Test.
-- | Syntax for Task, Card and Dir in KarelSyntax.hs
-- data Test = Not    Test   -- boolean negation
        --  | Facing Card   -- am I facing the given cardinal direction?
        --  | Clear  Dir    -- can I move in the given relative direction?
        --  | Beeper        -- is there a beeper here?
        --  | Empty         -- is my beeper bag empty?
--  deriving (Eq,Show)
-- data Card = North | South | East  | West  deriving (Eq,Show)

-- | Directions relative to the current facing.
-- data Dir  = Front | Back  | Right | Left  deriving (Eq,Show)

test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)

-- For Facing we will want ot use getFacing
-- getFacing takes a Robot which is r below
-- | The robot's facing (cardinal direction).
-- getFacing :: Robot -> Card
-- getFacing (_,c,_) = c
test (Facing c) _ r = c == (getFacing r)

-- For Clear Dir we will use the RelativePos helper Function and isClear function
-- This will give us a position relative to the robot's current facing and position
-- And the isClear will check if the position is clear. Supply RelativePos to isClear
-- relativePos :: Dir -> Robot -> Pos
-- relativePos d (p,c,_) = neighbor (cardTurn d c) p
-- | Is the given position clear?
-- isClear :: Pos -> World -> Bool
-- isClear p w = w p /= Nothing
-- type World = Pos -> Maybe Int
test (Clear d) w r = isClear (relativePos d r) w

-- We will use the hasBeeper helper function
-- | Is there a beeper at the given position?
-- hasBeeper :: Pos -> World -> Bool
-- hasBeeper p w = maybe False (>0) (w p)
-- We need to supply hasBeeper with a Pos so we can use a getPos helper function
-- | The robot's position.
-- getPos :: Robot -> Pos
-- getPos (p,_,_) = p
test Beeper w r = hasBeeper (getPos r) w

-- For Empty we will use the isEmpty helper function
-- | Is the beeper bag empty?
-- isEmpty :: Robot -> Bool
-- isEmpty (_,_,b) = b <= 0
test (Empty) _ r = isEmpty r


-- | Statements.
--data Stmt = Shutdown                 -- end the program
        --  | Move                     -- move forward
        --  | PickBeeper               -- take a beeper
        --  | PutBeeper                -- leave a beeper
        --  | Turn    Dir              -- rotate in place
        --  | Call    Macro            -- invoke a macro
        --  | Iterate Int  Stmt        -- fixed repetition loop
        --  | If      Test Stmt Stmt   -- conditional branch
        --  | While   Test Stmt        -- conditional loop
        --  | Block   [Stmt]           -- statement block
  --deriving (Eq,Show)
  -- | A macro name.
  --type Macro = String
  -- | A list of macro definitions.
  --type Defs = [(Macro,Stmt)]
-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move _ w r		  = let p = relativePos Front r
                            in if isClear p w
                                  then OK w (setPos p r)
                                  else Error ("Blocked at: " ++ show p)
stmt PickBeeper _ w r = let p = getPos r
                        	in if hasBeeper p w
                              	  then OK (decBeeper p w) (incBag r)
                              	  else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                            in if isEmpty r
                                  then Error ("No beeper to put.")
                                  else OK (incBeeper p w) (decBag r)
stmt (Turn d) _ w r       = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Call m) d w r       = case lookup m d of
                                 (Just a) -> stmt a d w r
                                 _        -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) d w r  = if i > 0
                               then case stmt s d w r of
                                         (OK w' r') -> stmt (Iterate (i-1) s) d w' r'
                                         (Done r')  -> Done r'
                                         (Error e)  -> Error e
                               else OK w r
stmt (If t s1 s2) d w r   = if (test t w r)
                                then stmt s1 d w r
                                else stmt s2 d w r
stmt (While t s) d w r    = if (test t w r)
                               then case stmt s d w r of
                                    (OK w' r') -> stmt (While t s) d w' r'
                                    (Done r')  -> Done r'
                                    (Error e)  -> Error e
                               else OK w r
stmt (Block []) _ w r     = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                                 (OK w' r') -> stmt (Block ss) d w' r'
                                 (Done r')  -> Done r'
                                 (Error e)  -> Error e
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
