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
_HEADER_ = _SEP_ ++  intercalate "" (  map (\i -> show i ++ _SEP_) _RANGE_ )

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

hasWon = undefined

-- Q#09

getGameState = undefined


playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined