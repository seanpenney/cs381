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
type State = (Macros, Stack)

data Prom = P Prog2
			| M Macros
			deriving Show

type S = State -> State


sem2 :: Prog2 -> S
sem2 [] a = a
sem2 (x:xs) a = sem2 xs (semCmd2 x a)

semCmd2 :: Cmd2 -> S
semCmd2 (LD2 i) (mac, stack) = (mac, i:stack)
semCmd2 (ADD2) (mac, stack) = (mac, ((stack !! 0 + stack !! 1):(drop 2 stack)))
semCmd2 (MULT2) (mac, stack) = (mac, ((stack !! 0 * stack !! 1):(drop 2 stack)))
semCmd2 (DUP2) (mac, stack) = (mac, ((stack !! 0):stack))
semCmd2 (DEF command list) (mac, stack) = ([(command, list)] ++ mac, stack)
semCmd2 (CALL command) (mac, stack) | command == "ADD2" = (mac, (snd (semCmd2 ADD2 (mac, stack))))
				| command == "MULT2" = (mac, (snd (semCmd2 MULT2 (mac, stack))))
				| command == "DUP2" = (mac, (snd (semCmd2 DUP2 (mac, stack))))

eval2 :: Prog2 -> State
eval2 p = sem2 p ([], [])

test20 = [LD2 3,DUP2,ADD2,DUP2,MULT2] -- gives [36]


-- Exercise 3, Mini Logo

data Cmd3 = Pen Mode
		| MoveTo Int Int
		| Seq Cmd3 Cmd3
		deriving Show
			
data Mode = Up | Down
			deriving (Show, Eq)

type State2 = (Mode,Int,Int)

type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd3 -> State2 -> (State2, Lines)
semS (Pen m) (mode, x, y)  = ((m, x, y), [])
semS (MoveTo x y) (mode, x2, y2) = ((mode, x, y), [(x2, y2, x, y)])

sem' :: Cmd3 -> Lines
sem' command = snd (semS command (Up, 0, 0))

test4 = MoveTo 1 2
