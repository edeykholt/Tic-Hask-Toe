module A3 where

import A1
import A2
import Data.List (intercalate, elemIndex)

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01
-- takes a list of `Int` values and returns a list of strings.
showInts :: [Int] -> [String]
showInts [] = []
showInts [i] = [show i]
showInts (i:is) = [show i] ++ showInts is

_HEADER_ = formatLine (showInts _RANGE_)

-- Q#02
-- takes a list of `Square` values and produces a list of strings.
showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = [showSquare x] ++ showSquares xs


-- Q#03
-- takes a list of `Row` values and produces a list of formatted strings by recursively applying  `showSquares` and `formatLine` to each row.
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x:xs) = formatLine (showSquares x) : formatRows xs

-- Q#04
-- takes a `Row` and an `Int` value corresponding to a column index and returns a boolean value indicating whether the corresponding square is empty.
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty row index = index < _SIZE_ && row !! index == E

-- Q#05
-- take a `Board` value and return a new `Board` with one column removed.
dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol [row] = [tail row]
dropFirstCol (row:rows) = tail row : dropFirstCol rows

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol [row] = [init row]
dropLastCol (row:rows) = init row : dropLastCol rows
-- Q#06
-- take a `Board` value and return a `Line` value corresponding to one of the diagonal lines on the board.
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 [row] = [head row] -- operating on the last row on the input Board
getDiag1 (row:rows) = head row : getDiag1 (dropFirstCol rows)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 [row] = [last row] -- operating on the last row on input Board
getDiag2 (row:rows) = last row : getDiag2 (dropLastCol rows)

getAllLines :: Board -> [Line]
getAllLines board = board ++ transpose board ++ [getDiag1 board] ++ [getDiag2 board]

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined