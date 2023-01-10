module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01
printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo =  readFile _LOGO_PATH_ >>= putStrLn 

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO ()
-- firstPlayer =  _RANDOM_BOOL_ >>= (\b -> print (getFirstPlayer b))
firstPlayer = _RANDOM_BOOL_ >>= \b -> print $ getFirstPlayer b

-- Q#04
getMove :: Board -> IO Move
getMove board = getLine >>= \moveString -> validateMove board moveString
    where 
        validateMove :: Board -> String -> IO Move
        validateMove b m
            | isValidMove b move = return move
            | otherwise          = print "Invalid move! Try again"  >> getMove b
            where move = stringToMove m
        
-- Q#05
play :: Board -> Player -> IO ()
play b p = 
    when _DISPLAY_LOGO_ printLogo >>
    printBoard b >>
    print (promptPlayer p) >>
    getMove b >>= -- TODO use results and pass move to next
    \move ->
        return ( playMove p b move ) >>= 
        \(gameState,newBoard) -> 
            if gameState == XWon || gameState == OWon || gameState == Tie
                then
                    print (showGameState gameState) >>
                    printBoard newBoard
                else
                    play newBoard $ switchPlayer p 

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined