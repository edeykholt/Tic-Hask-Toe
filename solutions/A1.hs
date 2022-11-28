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



-- type Square = Char
  -- Eq (Label s)
  -- Square x => Show (Label y)
-- type X = Square 
-- type O = Square
-- type B = Square

data Square = E | X | O 
  deriving (Eq, Show)  

-- instance Show Square where
  -- show :: String -> String
  -- show X = "X"
  -- show O = "O"
  -- show E = "_"

-- Q#07
data GameState


-- Q#08






-- Q#09

getFirstPlayer = undefined


getFirstPlayer_ = undefined

-- Q#10

showGameState gs = undefined

-- Q#11

switchPlayer = undefined


-- Q#12

showSquare = undefined