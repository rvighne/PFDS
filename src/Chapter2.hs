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

newtype UnbalancedSet a = UnbalancedSet (Tree a)
newtype CandidateSet a = CandidateSet (Tree a)
newtype SharingSet a = SharingSet (Tree a)

instance Set UnbalancedSet where
	empty = UnbalancedSet E

	member x (UnbalancedSet t) = member' t
		where
			member' E = False
			member' (T left y right)
				| x < y = member' left
				| x > y = member' right
				| otherwise = True

	insert x (UnbalancedSet t) = UnbalancedSet $ insert' t
		where
			insert' E = T E x E
			insert' s@(T left y right)
				| x < y = T (insert' left) y right
				| x > y = T left y (insert' right)
				| otherwise = s

-- 2.2
instance Set CandidateSet where
	empty = CandidateSet E

	member _ (CandidateSet E) = False
	member x (CandidateSet t@(T _ root _)) = member' t root
		where
			member' E p = x == p
			member' (T left y right) p
				| x < y = member' left p
				| otherwise = member' right y

	insert x (CandidateSet t)
		| UnbalancedSet t' <- insert x $ UnbalancedSet t = CandidateSet t'

-- 2.3
instance Set SharingSet where
	empty = SharingSet E

	member x (SharingSet t) = member x $ UnbalancedSet t

	insert x (SharingSet t) = SharingSet $ fromMaybe t $ insert' t
		where
			insert' E = Just $ T E x E
			insert' (T left y right)
				| x < y = (\h -> T h y right) <$> insert' left
				| x > y = (\h -> T left y h) <$> insert' right
				| otherwise = Nothing
