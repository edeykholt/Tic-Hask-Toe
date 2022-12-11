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
-- isNoEmpties :: Board -> Bool
boardAsSquares :: Board -> [Square]
boardAsSquares b = concat b

isBoardComplete :: Board -> Bool
isBoardComplete b = E `notElem` boardAsSquares b

-- count the number of squares of provided datatype.
countSquareData :: Square -> [Square] -> Int
countSquareData a [] = 0
countSquareData a (b : bs)
    | a==b = 1 + countSquareData a bs
    | otherwise = countSquareData a bs

countSquaresInBoard :: Square -> Board -> Int
countSquaresInBoard a b = countSquareData a (boardAsSquares b)

-- A game is tied if it contains no empty squares.
isTied b = isBoardComplete b
-- isTied :: Board -> Bool
-- isTied b = countSquaresInBoard X b  == countSquaresInBoard O b
        

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
        [X, O, O]
      , [O, X, X]
      , [O, X, O]
      ]

_INCOMPLETE_BOARD_ :: Board
_INCOMPLETE_BOARD_ = [
        [X, O, E]
      , [O, X, X]
      , [O, X, O]
      ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings a = zip ['A'.. ] a

-- Q#07
formatLine :: [String] -> String
formatLine s = _SEP_ ++ myLine ++ _SEP_
    where myLine = intercalate _SEP_ s

-- *** Assignment 2-2 *** --

-- Q#08
-- takes a `Move` a input. It should return a boolean value indicating whether the `Move`'s coordinates fall within the dimensions of the game board.
isMoveInBounds :: (Int, Int) -> Bool
isMoveInBounds (row, col) = isOnBoard row && isOnBoard col
    where isOnBoard value = value <= _SIZE_ && value >= 0

-- Q#09
-- takes a move in string format (i.e. `"B1"`) and returns a corresponding `Move` value.
stringToMove :: [Char] -> (Int, Int)
stringToMove [] = _INVALID_MOVE_
stringToMove (rowChar:colChar:xs) =
    if not (isDigit colChar)
        || xs /= []
        || rowIndex < 0
        || rowIndex > _SIZE_
        || colIndex < 0
        || colIndex > _SIZE_ 
    then  _INVALID_MOVE_ 
    else (rowIndex, colIndex)
    where 
        rowIndex :: Int
        rowIndex = convertRowIndex rowChar
        colIndex :: Int
        colIndex = readDigit colChar
stringToMove [_] = _INVALID_MOVE_

-- Q#10
-- takes an `Player` value, an `Int` value specifying a column index, and a `Row` value. It will return a new row with the square at the specified column replaced with the active player's tick mark.
rsX :: Int -> Row -> Row
rsX colIndex oldRow = rsSquare X colIndex oldRow 

rsO :: Int -> Row -> Row
rsO colIndex oldRow = rsSquare O colIndex oldRow 

rsSquare :: Square -> Int -> Row -> Row
rsSquare square colIndex oldRow 
    | null oldRow        = []
    | colIndex == 0      = square : snd (splitAt (colIndex+1) oldRow )
    | colIndex <0        = []
    | colIndex >= _SIZE_ = oldRow
    | otherwise          = fst (splitAt colIndex oldRow) ++ [square] ++ snd (splitAt (colIndex+1) oldRow)

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player colIndex oldRow = rsSquare player colIndex oldRow
