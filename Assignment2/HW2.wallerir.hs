module HW2 where

import Data.Maybe
import Prelude hiding (Num )

-- Ryan Wallerius ONID: wallerir
-- Daniyal Abbas  ONID: abbasd
-- PART 1 OF HW 2

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--

-- I used stack overflow for as a resource. Instead of using Empty I used End on line 53
-- This is because of how the Tree is constructed and what the assignment description says
-- https://stackoverflow.com/questions/30630873/map-function-to-a-bst-in-haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (End) = End
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt p (End) = Nothing
valueAt [] (Node a l r) = Just a
valueAt (x:xs) (Node a l r) = if x == L then valueAt xs l else valueAt xs r

-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
-- implementation  of pathTo using a feature
-- recursive calls using a pair.
-- If the value is found in the left branch, the subexpression
-- ‘pathTo x r’ will never be evaluated.
-- called “pattern guards”. The basic idea is that a guard
-- of the form ‘<pattern> <- <expr>’ will succeed if the
-- result of the expression matches the pattern and fail
-- otherwise (and then the next guard will be tried).
-- The code below is what we had but we could not get it working
--

--pathTo :: Eq a => a -> Tree a -> Maybe Path
--pathTo x (End) = Nothing
--pathTo x (Node y l r)
    -- | x == y = Just []
    -- | Just p <- pathTo x l = Just (L:p)
    -- | Just p <- pathTo x r = Just (R:p)


-- Part 2 of HW2
-- We will be dealing with the MiniLogo language which is defined by the following grammar:
-- num    ::= (any natural number)
-- var    ::= (any variable name)
-- macro  ::= (any macro name)

-- prog   ::= € | cmd;prog         sequence of commands
-- mode   ::= down | up            pen status

-- expr   ::= var                  variable reference
--          | num                  literal number
--          | expr+expr            addition expression

-- cmd    ::= pen mode             change pen mode
--          | move ( expr , expr ) move pen to a new position
--          | define macro ( var* ) { prog }  Define a macro
--          | call macro ( expr* )  invoke a macro


-- 1) Define the abstract syntax of MiniLogo as set of Haskell data types
-- Num, Var and Macro definition based on Assignment description
-- num    ::= (any natural number)
-- var    ::= (any variable name)
-- macro  ::= (any macro name)
type Num = Int
type Var = String
type Macro = String

-- Program definition based on Assignment description
-- prog   ::= € | cmd;prog         sequence of commands
type Prog = [Cmd]

-- Definition of Mode
-- mode   ::= down | up            pen status
data Mode = Down | Up
  deriving (Eq, Show)

-- Definition of expr
-- expr   ::= var                  variable reference
--          | num                  literal number
--          | expr+expr            addition expression
data Expr = Variable Var
          | Num Int
          | Add Expr Expr
          deriving (Eq, Show)

-- Definition of Cmd
-- cmd    ::= pen mode             change pen mode
--          | move ( expr , expr ) move pen to a new position
--          | define macro ( var* ) { prog }  Define a macro
--          | call macro ( expr* )  invoke a macro
-- I was getting an error when I had Move [Expr Expr]. Removing the [] solved the issue for number 2.
data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving (Eq, Show)

-- 2) Define a MiniLogo macro line that draws a line segment from (x1,y1) to (x2,y2)
--		MiniLogo concrete syntax:
--		define line (X1, Y1, X2, Y2){
--			pen up;
--			move (x1, y1);
--			pen down;
--			move (x2, y2)	}

line :: Cmd
line = Define "line" ["x1","y1","x2","y2"]
     [Pen Up,
     Move (Variable "x1") (Variable "y1"),
     Pen Down,
     Move (Variable "x2") (Variable "y2"),
     Pen Up]


-- 3) Use the line macro to define new MiniLogo macro nix
-- nix (x,y,w,h) that draws a big "X" of width w and height h, from
-- starting position (x,y). Can't contain move commands
--		MiniLogo concrete syntax:
--		define nix (x, y, w, h){
--			pen down;
--			line (x, y, x + w, y + h);
--			pen up;
--			line (x + w, y, x, y + h);	}
--
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
  [Call "line" [(Variable "x"), (Variable "y"), Add (Variable "x") (Variable "w"), Add (Variable "y") (Variable "h")],
	Call "line" [Add (Variable "x") (Variable "w"), (Variable "y"), (Variable "x"), Add (Variable "y") (Variable "h")]]

-- 4) Define Haskell function Steps
-- steps :: Int -> Prog that constructs a MiniLogo program that draws a staircase of n steps starting from (0,0). You may assume that n ≥ 0.
--		>>> steps 0
--		>>> steps 1
--
steps :: Int -> Prog
steps 0 = []
steps n = [
   Call "line" [Num n, Num n, Num (n-1), Num n],
   Call "line"[Num (n-1),  Num n, Num (n-1), Num (n-1)]
   ] ++ steps (n-1)

-- 5) Define haskell function macros
--
--		>>> macros []
--		[]
--		>>> macros [Pen Up]
--		>>> macros [Pen Up, Define "test1" [] []]
--		["test1"]
--		>>> macros [Pen Up, Define "test1" [] [], Pen Up, Define "test2" [] []]
--		["test1", "test2"]
--
macros :: Prog -> [Macro]
macros  [] = []
macros  (x:xs)
  | (ismacro x) == True = macroName x : macros xs
  | otherwise = macros xs

-- To check if the Minilog command is macro definition or not.
ismacro :: Cmd -> Bool
ismacro (Define _ _ _ ) = True
ismacro _ = False

-- Returns the macro name
macroName :: Cmd -> Macro
macroName (Define m _ _) = m
