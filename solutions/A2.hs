{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate, elemIndex)

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
-- take a character as input and return a boolean value True if a real value.
isDigit :: Char -> Bool
lChars = ['0' .. '9']
isDigit x = x `elem` lChars

-- takes a character value and returns the corresponding `Int` value if it's a valid digit. If a non-digit character is given, return `-1` as a default value.
readDigit :: Char -> Int
readDigit c = 
    if isDigit c
        then
            -- let cString = singleton c
            -- TODO reimplement along lines of hints in question
            convertRowIndex c + 17
        else -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

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