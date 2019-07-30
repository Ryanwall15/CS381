module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--
-- Ryan Wallerius: ONID: wallerir
-- Daniyal Abbas ONID: abbasd
-- Date: 07-29-2019
-- HW 3

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--

-- cmd (Pen Mode) (mode, point) = ((mode, point), Maybe Line)
-- cmd:: Cmd    ->      State           State        Maybe Line
-- cmd will be in the form of: Pen mode | Move Int Int
-- The "Base cases" for this follow this format

cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (_, poi) = ((Up, poi), Nothing) --Base case for Up
cmd (Pen Down) (_, poi) = ((Down, poi), Nothing) --Base case for Down
cmd (Move x1 y1) (m, (x2, y2)) = case m of -- Just like in class for cmd equ and add
                              Up   -> ((Up, (x1, y1)), Nothing)
                              Down -> ((Down, (x1, y1)), Just ((x2, y2),(x1, y1)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--
prog :: Prog -> State -> (State, [Line])
prog [] st = (st, [])
prog (h:t) st  = case cmd h st of
    (x, Just he) -> (\(st, t) -> (st, he:t)) $ prog t x
    (x, Nothing) -> prog t x

--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = ?

square:: Int -> Int -> Prog
square x z = [Pen Up, Move x z, Pen Down, Move (x+1) (z), Move  (x+1) (z +1), Move  (x) (z+1), Move x z]
