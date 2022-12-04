{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01
-- takes a `Player` value and returns a string prompting the given player to enter a row and column position.
promptPlayer :: Player -> String
promptPlayerPrefix = "Player "
promptPlayerPostfix = "'s turn: enter a row and column position (ex. A1)"
realPlayerPrompt p = concat [promptPlayerPrefix, show p, promptPlayerPostfix]
promptPlayer p = 
    case p of
        X -> realPlayerPrompt p
        O -> realPlayerPrompt p
        _ -> "No turn available for empty space or player"
    
-- Q#02
_RANGE_ = [0 .. _SIZE_-1]

-- Q#03

isDigit = undefined


readDigit = undefined

-- Q#04

_EMPTY_ROW_ = undefined


_EMPTY_BOARD_ = undefined

-- Q#05

isTied = undefined


_TIED_BOARD_ = undefined

-- Q#06

indexRowStrings = undefined

-- Q#07

formatLine = undefined

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined