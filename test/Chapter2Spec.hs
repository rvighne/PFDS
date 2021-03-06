{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE Rank2Types #-}
module Chapter2Spec where

import Chapter2

import Test.QuickCheck

import Prelude hiding (lookup)
import Data.List (tails)
import Data.Maybe
import Data.Bifunctor
import Control.Monad

deriving instance Functor Tree
deriving instance Foldable Tree
deriving instance Eq a => Eq (Tree a)
deriving instance Show a => Show (Tree a)

deriving instance Show a => Show (SharingSet a)
deriving instance Show a => Show (CandidateSet a)
deriving instance (Show k, Show v) => Show (UnbalancedMap k v)

instance (Ord a, Arbitrary a) => Arbitrary (SharingSet a) where
	arbitrary = oneof [return empty, liftM2 insert arbitrary arbitrary]

instance (Ord a, Arbitrary a) => Arbitrary (CandidateSet a) where
	arbitrary = oneof [return empty, liftM2 insert arbitrary arbitrary]

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (UnbalancedMap k v) where
	arbitrary = oneof [return emptyMap, liftM3 bind arbitrary arbitrary arbitrary]

-- 2.1
prop_suffixesIsTails :: Eq a => [a] -> Bool
prop_suffixesIsTails xs = suffixes xs == tails xs

sameAsUB :: (BSTSet s, Eq r) => (forall z. BSTSet z => z a -> r) -> s a -> Bool
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
prop_completeNodeCount :: Integral n => NonNegative n -> Property
prop_completeNodeCount (NonNegative d) = d < 16 ==> (length $ complete () d) == 2 ^ d - 1

-- 2.5(b)
prop_isBalanced :: Integral n => NonNegative n -> Bool
prop_isBalanced = isJust . depth . balanced () . getNonNegative
	where
		depth E = Just 0
		depth (T left _ right)
			| Just ld <- (depth left)
			, Just lr <- (depth right)
			= if abs (ld - lr) <= 1 then Just (1 + max ld lr) else Nothing
			| otherwise = Nothing

-- 2.6
prop_lookupSameAsBound :: (Ord k, Eq v) => k -> v -> UnbalancedMap k v -> Bool
prop_lookupSameAsBound k v m = lookup k (bind k v m) == Just v

-- 2.6
prop_lookupNonExistent :: Integral k => Negative k -> UnbalancedMap (NonNegative k) v -> Bool
prop_lookupNonExistent (Negative q) (UnbalancedMap t) = isNothing $ lookup q $ UnbalancedMap $ first getNonNegative <$> t

return []
runTests :: IO Bool
runTests = $quickCheckAll
