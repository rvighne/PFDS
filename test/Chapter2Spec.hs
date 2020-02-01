{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Chapter2Spec where

import Chapter2

import Test.QuickCheck

import Data.List (tails)
import Control.Monad (liftM2)

deriving instance Eq a => Eq (Tree a)

deriving instance Show a => Show (SharingSet a)
deriving instance Show a => Show (CandidateSet a)

instance (Ord a, Arbitrary a) => Arbitrary (SharingSet a) where
	arbitrary = oneof [return empty, liftM2 insert arbitrary arbitrary]

instance (Ord a, Arbitrary a) => Arbitrary (CandidateSet a) where
	arbitrary = oneof [return empty, liftM2 insert arbitrary arbitrary]

-- 2.1
prop_suffixesIsTails :: Eq a => [a] -> Bool
prop_suffixesIsTails xs = suffixes xs == tails xs

sameAsUB :: (BST s, Ord a, Eq r) => (forall z. BST z => z a -> r) -> s a -> Bool
sameAsUB f s = (f s) == (f $ UnbalancedSet $ getTree s)

-- 2.2
prop_candidateMemberIsUBMember :: Ord a => a -> CandidateSet a -> Bool
prop_candidateMemberIsUBMember x = sameAsUB $ member x

-- 2.3
prop_sharingInsertIsUBInsert :: Ord a => a -> SharingSet a -> Bool
prop_sharingInsertIsUBInsert x = sameAsUB $ getTree . insert x

-- 2.4
prop_candidateInsertIsUBInsert :: Ord a => a -> CandidateSet a -> Bool
prop_candidateInsertIsUBInsert x = sameAsUB $ getTree . insert x

-- 2.5(a)
prop_completeNodeCount :: Gen Bool
prop_completeNodeCount = do
	d <- choose (0, 10) :: Gen Integer
	return $ (count $ complete undefined d) == 2 ^ d - 1
	where
		count E = 0
		count (T left _ right) = (count left) + 1 + (count right)

return []
runTests :: IO Bool
runTests = $quickCheckAll
