-- CS 381, Homework 4
-- Sean Penney, Paul Atkinson, Kelson Luc

--Question 1

--[]
--[x:?]									push
--[y:?, x:?]							push
--[f{}, y:?, x:?]						push
--[x:2, f{}, y:?, x:?]					push
--[x:1, x:2, f{}, y:?, x:?]				push
--[x:0, x:1, x:2, f{}, y:?, x:?]		push
--[x:0, x:1, x:2, f{}, y:1, x:?]		push
--[x:1, x:2, f{}, y:2, x:?]				pop
--[x:2, f{}, y:4, x:?]					pop
--[f{}, y:?, x:4]						pop
--[y:?, x:4]							pop
--[]									pop

--Question 2

--a. If it is static, then the answer is 33.
--b. If it is dynamic, then the answer is 26