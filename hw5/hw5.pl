/* Paul Atkinson, Sean Penney, Kelson Luc */ 

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

/* Part A: S = Student, P = Place, T = Time */
schedule(S, P, T) :- enroll(S, C), when(C, T), where(C, P).

/* Part B: P = Place, T = Time */
usage(P, T) :- where(C, P), when(C, T).

/* Part C: C = Class1, X = Class2 */
conflict(C, X) :- when(C, T), when(X, T), where(C, P), where(X, P), C\=X.

/* Part D: S1 = Student 1, S2 = Student 2*/
meet(S1, S2) :- schedule(S1, P, T), schedule(S2, P, T), S1\=S2; schedule(S1, P, T1), schedule(S2, P, T2), T1\==T2+1, S1\=S2.
