-- CS 381, Homework 1
-- Sean Penney, Paul Atkinson, Kelson

--module hw1 where

-- Exercise 1, Mini Logo

--data Cmd = Pen Mode
--		| Moveto Pos Pos
--		| Def Name Pars Cmd
--		| Call Name Vals
		| Commands [Cmd]
		
--data Mode = Up | Down

--data Pos = Num | Name

--data Pars = [Name]

--data vals = [Num]

--type Name = String

--type Num = Int


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
--In the second representation,

--c)
translate :: Expr -> Exxp
translate(N x) = Num x
translate(Plus x y) = Apply Add[translate(x), translate(y)]