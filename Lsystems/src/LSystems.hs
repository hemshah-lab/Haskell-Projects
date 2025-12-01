module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2
                , expandLSystem ) where

import IC.Colour
import Graphics.UI.GLUT (Color(color))

-- Part 1 - Warmup
----------------------------------------------------------
type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)

-- Functions for working with systems. Skeleton optional...

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem a _ _) = a

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ x _) = x

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ r) = r

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar cvs a = concat [ v | (c, v) <- cvs , c == a]

-- Part 2: Expansion and Parsing
data Command = F | L | R | B [Command] deriving Show

--
-- Expand command string s once using rule table r
--

-- expandOne :: Rules Char -> [Char] -> [Char]
-- expandOne _ [] = []
-- expandOne cvs (r:rs) = lookupChar cvs r ++ expandOne cvs rs

expandOne :: Rules Char -> [Char] -> [Char]
expandOne cvs = foldr replace "" 
    where
        replace :: Char -> [Char] -> [Char]
        replace r racc = lookupChar cvs r ++ racc
--
-- Expand command string s n times using rule table r
--
expand :: Rules Char -> [Char] -> Int -> [Char]
expand _ rs 0 = rs
expand cvs rs n = foldl replace rs [1..n]
    where
        replace :: [Char] -> Int -> [Char]
        replace racc _ = expandOne cvs racc      

parse :: [Char] -> [Command]
parse [] = []
parse ('[':cs) = B (parse block) : parse rest
  where
    (block, rest) = splitBracket cs
    splitBracket :: [Char] -> ([Char], [Char])
    splitBracket = go 1 []
      where
        go 0 acc rest = (reverse acc, rest)
        go _ acc [] = (reverse acc, [])
        go n acc (x:xs)
          | x == '['  = go (n + 1) (x:acc) xs
          | x == ']'  = go (n - 1) (x:acc) xs
          | otherwise = go n (x:acc) xs
parse (']':cs) = parse cs 
parse (c:cs) = lookupChar commandMap c ++ parse cs

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem system n = parse (expand (rules system) (axiom system) n)

-- Part 3: Turtles
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
type ColouredLine = (Vertex, Vertex, Colour)

initialState :: TurtleState
initialState = ((0, 0), 90)

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.

move :: Command -> Float -> TurtleState -> TurtleState
move L ang ((x, y), bear) = ((x, y), bear + ang)
move R ang ((x, y), bear) = ((x, y), bear - ang)
move F ang ((x, y), bear) = ((x', y'), bear)
    where
        x' = x + cos(degreesToRadians bear)
        y' = y + sin(degreesToRadians bear)


trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 [] _ _ = []
trace1 cmds ang colr = go cmds initialState
    where
        go :: [Command] -> TurtleState -> [ColouredLine]
        go [] _ = []
        go ( F : cmds') state = (fst state, fst newState, colr) : go cmds' newState
            where
                newState = move F ang state
        go ( B sub : cmds') state = branchcmds ++ aftercmds
            where
                branchcmds = go sub state
                aftercmds = go cmds' state
        go ( c : cmds') state = go cmds' newState
            where
                newState = move c ang state


-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 cmds ang colr = go cmds initialState []
    where
        go :: [Command] -> TurtleState -> [(TurtleState, [Command])] -> [ColouredLine]
        go (B c : cmds') state stack = go c state ((state, cmds') : stack)
        go (F : cmds') state stack = (fst state, fst newState, colr) : go cmds' newState stack
            where
                newState = move F ang state
        go ( c : cmds') state stack = go cmds' newState stack
            where
                newState = move c ang state
        go [] state ((stackState, stackCmds) : stack) = go stackCmds stackState stack
        go [] _ [] = []


-- Provided Helper Functions
------------------------------------------------------------------------------

degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
