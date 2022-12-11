module Sandbox2 where
import Data.Bits (Bits(xor))

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