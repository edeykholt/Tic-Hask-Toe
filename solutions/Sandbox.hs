-- {-# OPTIONS_GHC -Who-unrecognised-pragmas #-}
-- {-# HLINT ignore "use foldr" #-}

module Sandbox where
import Data.Bits (Bits(xor))
-- import A1
-- import Data.List (intercalate, elemIndex)

-- import Prelude hiding (sum, concat)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- context arrow => 
-- sum takes a list of a's and returns an a, where a is of typeClass Num
sum'' :: Num a => [a] -> a
sum'' []    = 0
sum'' (a:as) = a + sum' as

concat' :: [[a]] -> [a]
concat' (xs : xss) = xs ++ concat' xss 
concat' []         = []

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- x `elem` xs
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _ = []

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x : init' xs
