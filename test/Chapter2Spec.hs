{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Chapter2Spec where

import Chapter2

import Test.QuickCheck

import Data.List (tails)
import Control.Monad (liftM2)

deriving instance Eq a => Eq (Tree a)

deriving instance Show a => Show (Tree a)
deriving instance Show a => Show (CandidateSet a)
deriving instance Show a => Show (SharingSet a)

instance (Ord a, Arbitrary a) => Arbitrary (CandidateSet a) where
	arbitrary = oneof [return empty, liftM2 insert arbitrary arbitrary]

instance (Ord a, Arbitrary a) => Arbitrary (SharingSet a) where
	arbitrary = oneof [return empty, liftM2 insert arbitrary arbitrary]

prop_suffixesIsTails :: Eq a => [a] -> Bool
prop_suffixesIsTails xs = suffixes xs == tails xs

prop_candidateMemberIsMember :: Ord a => a -> CandidateSet a -> Bool
prop_candidateMemberIsMember x s@(CandidateSet t) = member x s == member x (UnbalancedSet t)

prop_sharingInsertIsInsert :: Ord a => a -> SharingSet a -> Bool
prop_sharingInsertIsInsert x s@(SharingSet t)
	| SharingSet s' <- insert x s
	, UnbalancedSet u' <- insert x (UnbalancedSet t)
	= s' == u'

return []
runTests :: IO Bool
runTests = $quickCheckAll
