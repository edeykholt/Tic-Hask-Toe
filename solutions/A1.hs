module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = False

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex a = ( fromEnum ( toUpper a ) ) - 65

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: String
_SEP_ = "_|_"

-- *** Assignment 1-2 *** --
-- Q#06
data Square = E | X | O 
  deriving (Eq, Show)  

-- instance Show Square where
  -- show :: String -> String
  -- show X = "X"
  -- show O = "O"
  -- show E = "_"

-- Q#07
data GameState = XWon | OWon | Tie | InProgress
  deriving (Eq, Show)


-- Q#08
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [] Row
type Move = (Int, Int)

-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer a =
    if a 
      then X 
      else O
getFirstPlayer_ a
  | a = X
  | otherwise = O

-- Q#10
-- receives a `GameState` value and returns a string message describing the current game state.
showGameState :: GameState -> String
showGameState gs =
  case gs of 
    XWon -> "X Won"
    OWon -> "O Won"
    Tie -> "Tie"
    _ -> "InProgress"

-- Q#11
-- receives an `Player` value and returns a new `Player` value.
switchPlayer :: Player -> Player
switchPlayer p =
  case p of
    X -> O
    O -> X
    E -> E

-- Q#12
-- receives a `Square` value and returns a string representation of it 
showSquare :: Square -> String
showSquare s =
  case s of
    E -> "_"
    _ -> show s

