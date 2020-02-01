module Chapter2 where

-- 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes list@(_:suf) = list : suffixes suf
