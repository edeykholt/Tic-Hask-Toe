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
dropLastCol [row] = [take (_SIZE_-1) row]
dropLastCol (row:rows) = take (_SIZE_-1) row : dropLastCol rows
-- Q#06

getDiag1 = undefined


getDiag2 = undefined


getAllLines = undefined

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined