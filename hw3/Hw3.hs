-- CS 381, Homework 3
-- Sean Penney, Paul Atkinson, Kelson Luc

module Hw3 where

-- Exercise 1, A Rank-Based Type Systems for the Stack Language

--a)
type Prog = [Cmd]

data Cmd = LD Int
		| ADD
		| MULT
		| DUP
		| INC
		| SWAP
		| POP Int
		
semCmd :: Cmd -> D
semCmd (LD i) xs = i:xs
semCmd (ADD) xs = (xs !! 0 + xs !! 1):(drop 2 xs)
semCmd (MULT) xs = (xs !! 0 * xs !! 1):(drop 2 xs)
semCmd (DUP) xs = (xs !! 0):xs
semCmd (INC) xs = ((xs !! 0) + 1):(drop 1 xs)
semCmd (SWAP) xs = (xs !! 1):(xs !! 0):(drop 2 xs)
semdCmd (POP i) xs = (drop i xs)

sem :: Prog -> D
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

type Stack = [Int]
type D = Stack -> Stack
		
type Rank = Int
type CmdRank = (Int, Int)

rankC :: Cmd -> CmdRank
rankC (LD i) = (0, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 2)
rankC INC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP i) = (i, 0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r		| r >= 0 = Just r
rank (x:xs) r	| currenttotal >= 0 = rank xs (currenttotal + add)
				where		(sub, add) = rankC x
						currenttotal = r - sub
rank _ _ 		= Nothing

rankP :: Prog -> Maybe Rank
rankP xs = rank xs 0

--b)
typeSafe :: Prog -> Bool
typeSafe p = (rankP p) /= Nothing

semStatTC :: Prog -> Maybe Stack
semStatTC p | typeSafe p = Just (sem p ([]))
             | otherwise  = Nothing

--The type could be simplified by changing it from Prog -> Maybe Stack to just Prog -> Stack
--The type checker handles errors, so we do not need error handling in sem.

test1 = [LD 4, DUP, LD 1]

-- Exercise 2, Shape Language

--a)
data Shape = X
			| TD Shape Shape
			| LR Shape Shape
			deriving Show

type BBox = (Int,Int)
			
bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2) | s1x >= s2x = (s1x, s1y + s2y)
				| s1x < s2x = (s2x, s1y + s2y)
				where		(s1x, s1y) = bbox s1
						(s2x, s2y) = bbox s2
bbox (LR s1 s2) | s1y >= s2y = (s1x + s2x, s1y)
				| s1y < s2y = (s1x + s2x, s2y)
				where		(s1x, s1y) = bbox s1
						(s2x, s2y) = bbox s2

--b)
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD s1 s2) | Just s1x == Just s2x = Just(s1x, s1y + s2y)
				| otherwise = Nothing
				where		Just(s1x, s1y) = (rect s1)
						Just(s2x, s2y) = (rect s2)
rect (LR s1 s2) | Just s1y == Just s2y = Just(s1x + s2x, s1y)
				| otherwise = Nothing
				where		Just (s1x, s1y) = (rect s1)
						Just (s2x, s2y) = (rect s2)

test2 = TD X X

-- Exercise 3, Parametric Polymorphism

--a)
	--1. The return type of f and g are both lists, however, the types of x and y in f are the same and are different for g.
	--2. Y is contained in a list, so x has to be a list as well.
	--3. g would be more general, since x and y are not limited to be the same type.
	--4. They could be the same type, but g has more flexibility in the input types of x and y.
	
--b)
h xs ts = xs ++ (map snd ts)
--c)
--k a b = (even a) (if even a then a)

--d)
-- The type of a -> b is very difficult to define as haskell is strongly typed, variables of a type
-- can only be interpreted as that type. We could not find a simply way given one type to return 
-- another general type of b.