{-# LANGUAGE TemplateHaskell #-}
module Chapter2Spec where

import Chapter2

import Test.QuickCheck.All
import Data.List

prop_suffixesIsTails :: Eq a => [a] -> Bool
prop_suffixesIsTails xs = suffixes xs == tails xs

return []
runTests :: IO Bool
runTests = $quickCheckAll
