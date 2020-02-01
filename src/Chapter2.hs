{-# LANGUAGE TypeSynonymInstances #-}
module Chapter2 where

import Data.Maybe (fromMaybe)

-- 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes list@(_:suf) = list : suffixes suf

data Tree a = E | T (Tree a) a (Tree a)

class Set s where
	empty :: s a
	member :: Ord a => a -> s a -> Bool
	insert :: Ord a => a -> s a -> s a

type UnbalancedSet = Tree
newtype CandidateSet a = CandidateSet (UnbalancedSet a)
newtype SharingSet a = SharingSet (UnbalancedSet a)

instance Set UnbalancedSet where
	empty = E

	member _ E = False
	member x (T left y right)
		| x < y = member x left
		| x > y = member x right
		| otherwise = True

	insert x E = T E x E
	insert x t@(T left y right)
		| x < y = T (insert x left) y right
		| x > y = T left y (insert x right)
		| otherwise = t

-- 2.2
instance Set CandidateSet where
	empty = CandidateSet empty

	member _ (CandidateSet E) = False
	member x (CandidateSet t@(T _ root _)) = member' t root
		where
			member' E p = x == p
			member' (T left y right) p
				| x < y = member' left p
				| otherwise = member' right y

	insert x (CandidateSet t) = CandidateSet $ insert x t

-- 2.3
instance Set SharingSet where
	empty = SharingSet empty

	member x (SharingSet t) = member x t

	insert x (SharingSet t) = SharingSet $ fromMaybe t $ insert' t
		where
			insert' E = Just $ T E x E
			insert' (T left y right)
				| x < y = (\h -> T h y right) <$> insert' left
				| x > y = (\h -> T left y h) <$> insert' right
				| otherwise = Nothing
