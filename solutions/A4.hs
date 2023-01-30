module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )
import Data.List (intercalate)

-- *** Assignment 4-1 *** --

-- Q#01
-- _HEADER_ = String
_HEADER_ = " " ++ _SEP_ ++  intercalate "" (  map (\i -> show i ++ _SEP_) _RANGE_ )

-- Q#02
showSquares :: [Square] -> [String]
showSquares squares = map (\s -> showSquare s) squares

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol board = map (\row -> tail row) board

-- Q#04
dropLastCol :: Board -> Board
dropLastCol board = map (\row -> init row) board

--Q#05
formatRows :: [Row] -> [String]
formatRows rows =  map (\row -> formatLine (showSquares row)) rows 

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player line = null (filter (\cell -> cell /= player || cell == E) line) && line /= []


-- *** Assignment 4-2 *** --

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine player line = not (null line) && foldr (\cell acc -> acc && (cell == player)) True line

-- Q#08
-- function `hasWon` that takes a `Player` and a `Board` value as inputs and returns a boolean value indicating whether the player has a winning line anywhere in the board.
hasWon :: Player -> Board -> Bool
hasWon player board = foldr (\line acc -> acc || isWinningLine player line ) False (getAllLines board)

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09
getGameState :: Board -> GameState
getGameState board  
  | hasWon O board = OWon
  | hasWon X board = XWon
  | isTied board = Tie
  | otherwise = Playing

-- apply a player move and return both the resulting `GameState` and an updated board.
playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState newBoard, newBoard)
  where newBoard = putSquare p b m

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices ss = zipWith (\x y -> x : y) ['A' ..] ss

-- Q#11
formatBoard :: Board -> String
-- formatBoard b =   unlines (_HEADER_ : ( prependRowIndices $ formatRows b ))
-- formatBoard b =   unlines $ _HEADER_ : ( prependRowIndices $ formatRows b )
-- formatBoard b =   unlines $ _HEADER_ : ( prependRowIndices $ formatRows b )
-- formatBoard b =   unlines $ _HEADER_ : prependRowIndices (formatRows b)
formatBoard b = unlines . (_HEADER_ :) . prependRowIndices $ formatRows b
-- (\xs -> _HEADER_ : xs)
