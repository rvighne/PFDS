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

class Set t => BST t where
	getTree :: t a -> Tree a

newtype UnbalancedSet a = UnbalancedSet (Tree a)
instance BST UnbalancedSet where
	getTree (UnbalancedSet t) = t

newtype SharingSet a = SharingSet (Tree a)
instance BST SharingSet where
	getTree (SharingSet t) = t

newtype CandidateSet a = CandidateSet (Tree a)
instance BST CandidateSet where
	getTree (CandidateSet t) = t

instance Set UnbalancedSet where
	empty = UnbalancedSet E

	member x = member' . getTree
		where
			member' E = False
			member' (T left y right)
				| x < y = member' left
				| x > y = member' right
				| otherwise = True

	insert x = UnbalancedSet . insert' . getTree
		where
			insert' E = T E x E
			insert' s@(T left y right)
				| x < y = T (insert' left) y right
				| x > y = T left y (insert' right)
				| otherwise = s

instance Set SharingSet where
	empty = SharingSet E

	member x = member x . UnbalancedSet . getTree

	-- 2.3
	insert x (SharingSet t) = SharingSet $ fromMaybe t $ insert' t
		where
			insert' E = Just $ T E x E
			insert' (T left y right)
				| x < y = (\h -> T h y right) <$> insert' left
				| x > y = (\h -> T left y h) <$> insert' right
				| otherwise = Nothing

instance Set CandidateSet where
	empty = CandidateSet E

	-- 2.2
	member x = flip member' Nothing . getTree
		where
			member' E p = (Just x) == p
			member' (T left y right) p
				| x < y = member' left p
				| otherwise = member' right $ Just y

	-- 2.4
	insert x (CandidateSet t) = CandidateSet $ fromMaybe t $ insert' t Nothing
		where
			insert' E p
				| (Just x) /= p = Just $ T E x E
				| otherwise = Nothing
			insert' (T left y right) p
				| x < y = (\h -> T h y right) <$> insert' left p
				| otherwise = (\h -> T left y h) <$> insert' right (Just y)
