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

-- use inductive recursion??
-- zip?
-- _HEADER_ = intercalate ([" "] ++ showInts _RANGE_ )

-- Q#02

showSquares = undefined


-- Q#03

formatRows = undefined

-- Q#04

isColEmpty = undefined

-- Q#05

dropFirstCol = undefined


dropLastCol = undefined

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