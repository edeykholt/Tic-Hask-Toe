{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}


module Sandbox2 where
import Data.Bits (Bits(xor))

-- import Data.Foldable
import Data.List

reverse_ :: [a] -> [a]
reverse_ xs = go [] xs
 where
    go :: [a] -> [a] -> [a]
    go reversed [] = reversed
    go reversed (x:xs) = go (x : reversed) xs

    -- reverse "star" = go [] "star"
        -- go [] ('s' : "tar") => go ('s' : []) "tar"
        -- go "s" ('t' : "ar") => go ('t' : "s") "ar"
        -- go "ts" ('a' : "r") => go ('a' : "ts") "r"
        -- go "ats" ('r' : [] ) => go ('r' : "ats") [] => "rats"

sum' :: Num a => [a] -> a
sum' ns = foldr (+) 0 ns

product' :: Num a => [a] a
product' = foldr (*) 1

and' :: [Bool] -> Bool
and' bs = foldr (&&) True bs

or' :: [Bool] -> Bool
or' bs = foldr (||) False bs

elem' :: Eq a => a -> [a] -> Bool
elem' q es = foldr (\e b -> || e == q) False es

maximum' :: Ord a => [a] -> a 
maximum' [] = error "empty list"
maximum' (x:xs) = foldr max x xs

minimum' :: Ord a => [a] -> a 
minimum' [] = error "empty list"
minimum' (x:xs) = foldr min x xs

reverse' :: [a] -> [a]
reverse' es = foldr (\e reversed -> reversed [e]) [] es
-- above is inefficient.  "O(n)" See below...

reverse'' :: [a] -> [a]
reverse'' = foldl' (\reversed e -> e : reversed ) [] es
-- above is more efficient  "O(1)"

reverse''' es = foldl' (flip (:)) [] es
