-- CS 381, Homework 1
-- Sean Penney, Paul Atkinson, Kelson Luc

--module hw1 where

import Prelude hiding (Num)

-- Exercise 1, Mini Logo

--a)
data Cmd = Pen Mode
		| Moveto (Pos, Pos)
		| Def Name Pars Cmd
		| Call Name Vals
		| Seq [Cmd]
		deriving (Show)
		
		
data Mode = Up | Down
	deriving (Show)

data Pos = Foo Num | Bar Name deriving (Show)

type Pars = [Name]

type Vals = [Num]

type Name = String

type Num = Int

--b)
vector = Def "vector" ["x1", "y1", "x2", "y2"] (Seq [Pen Down, Moveto (Bar "x1", Bar "y1"), Moveto (Bar "x2", Bar "y2"), Pen Up])

--c)
steps :: Int -> Cmd
steps n | n < 1 = Seq []
		| n == 1 = Seq [Call "vector" [0, 0, 0, 1], Call "vector" [0, 1, 1, 1]] -- move from (0,0) to (1,1)
		| n > 1 = Seq [steps (n-1), Seq[Call "vector" [n-1, n-1, n-1, n], Call "vector" [n-1, n, n, n]]]

-- Exercsie 2, Digital Circuit Design Language

--a)
data Circuit = Circuit Gates Links

data Gates = Gate Int Gatefn Gates | GatesEmpty

data Gatefn = And | Or | Xor | Not
				deriving Show

data Links = Link Int Int Int Int Links | LinksEmpty
				

--b)
halfadder = Circuit (Gate 1 Xor (Gate 2 And GatesEmpty)) (Link 1 1 2 1 (Link 1 2 2 2 LinksEmpty))

--c)
--NOTE: In order to properly display format for question, a call to any pretty printer must be wrapped
--inside putStr, ie. putStr(ppCircuit halfadder)
ppCircuit :: Circuit -> String
ppCircuit (Circuit g l) = ppGates g ++ ppLinks l

ppGates :: Gates -> String
ppGates GatesEmpty = ""
ppGates (Gate n fn g) = show n ++ ":" ++ show fn ++ ";\n" ++ ppGates g

ppLinks :: Links -> String
ppLinks LinksEmpty = ""
ppLinks (Link a b c d l) = "from " ++ show a ++ "." ++ show b ++ " to " ++ show c ++ "." ++ show d ++ ";\n" ++ ppLinks l
	
-- Exercise 3, Designing Abstract Syntax

data Expr = N Int
		| Plus Expr Expr
		| Times Expr Expr
		| Neg Expr
		
data Op = Add | Multiply | Negate

data Exp = Num Int
		| Apply Op [Exp]

--a)		
result = Apply Multiply [Apply Negate[Apply Add[Num 3, Num 4]], Num 7]

--b)
--The first representation only requires one constructor, which makes it easier to read.

--In the first representation, one would be limited to using two numbers in the Plus and Times functions.
--In the second representation, any amount of numbers could be used.

--c)
translate :: Expr -> Exp
translate(N x) = Num x
translate(Plus x y) = Apply Add[translate(x), translate(y)]
translate(Times x y) = Apply Multiply[translate(x), translate(y)]
translate(Neg x) = Apply Negate [translate(x)]
