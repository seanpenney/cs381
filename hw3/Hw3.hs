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

test1 = [LD 4, DUP, ADD, LD 1]