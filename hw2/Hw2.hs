-- CS 381, Homework 2
-- Sean Penney, Paul Atkinson, Kelson Luc

module Hw2 where

-- Exercise 1, A Stack Language


type Prog = [Cmd]

data Cmd = LD Int
			| ADD
			| MULT
			| DUP
			deriving (Show)
			
type Stack = [Int]
			

type D = Stack -> Stack

semCmd :: Cmd -> D
semCmd (LD i) xs = i:xs
semCmd (ADD) xs = (xs !! 0 + xs !! 1):(drop 2 xs)
semCmd (MULT) xs = (xs !! 0 * xs !! 1):(drop 2 xs)
semCmd (DUP) xs = (xs !! 0):xs

sem :: Prog -> D
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

eval :: Prog -> Stack
eval p = sem p ([])

test1 = [LD 3, ADD] -- does not evalutate
test2 = [LD 3,DUP,ADD,DUP,MULT] -- gives [36]
test3 = [] -- gives empty list


-- Exercise 2, Extending the Stack Language by Macros

--a)
data Cmd2 = LD2 Int
			| ADD2
			| MULT2
			| DUP2
			| DEF String [Cmd2]
			| CALL String
			deriving (Show)
			
--b)
type Prog2 = [Cmd2]
type Macros = [(String,Prog2)]

-- Exercise 3, Mini Logo

data Cmd3 = Pen Mode
		| MoveTo Int Int
		| Seq Cmd3 Cmd3
		deriving Show
			
data Mode = Up | Down
			deriving (Show, Eq)

type State = (Mode,Int,Int)

type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd3 -> State -> (State, Lines)
semS (Pen m) (mode, x, y)  = ((m, x, y), []) 

--sem' :: Cmd3 -> Lines
