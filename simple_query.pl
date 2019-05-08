/* question 1*/
setUnion([],A,A).
setUnion([A|B],C,D):-member(A,C),!,setUnion(B,C,D).
setUnion([A|B],C,[A|D]):-setUnion(B,C,D).

/* question 2*/
swap([],[]).
swap([A,B|L],[B,A|R]) :- swap(L,R).
swap([A],[A]).

/* question 3*/
flatten([],[]).
flatten([A|L],[A|L1]) :-
     xatom(A), !, flatten(L,L1).
flatten([A|L],R) :-
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).
xatom(A) :- atom(A).
xatom(A) :- number(A).
     
max_l([X],X) :- !, true.
max_l([X|Xs], M):- max_l(Xs, M), M >= X.
max_l([X|Xs], X):- max_l(Xs, M), X >  M.

largest(A,D):-flatten(A,B),max_l(B,D).

/* question 4*/
countAll(List,A):-
    findall([X,L], (bagof(true,member(X,List),Xs), length(Xs,L)),A).

/* question 5*/
sub(A,[[B,C]|R],F):- !,subs(B,C,A,D),sub(D,R,F).
sub(A,[],A).
subs(_, _, [], []).
subs(X, Y, [X|T1], [Y|T2]) :- subs(X, Y, T1, T2),!.
subs(X, Y, [H|T1], [H|T2]) :- \+ is_list(H),!, subs(X, Y, T1, T2).
subs(X, Y, [H1|T1], [H2|T2]) :- subs(X, Y, H1, H2), subs(X, Y, T1, T2).

/* question 6*/
insert:-
assert(c325(fall_2010,john,14,13,15,10,76,87)),
assert(c325(fall_2010,lily, 9,12,14,14,76,92)),
assert(c325(fall_2010,peter,8,13,12,9,56,58)),
assert(c325(fall_2010,ann,14,15,15,14,76,95)),
assert(c325(fall_2010,ken,11,12,13,14,54,87)),
assert(c325(fall_2010,kris,13,10,9,7,60,80)),
assert(c325(fall_2010,audrey,10,13,15,11,70,80)),
assert(c325(fall_2010,randy,14,13,11,9,67,76)),
assert(c325(fall_2010,david,15,15,11,12,66,76)),
assert(c325(fall_2010,sam,10,13,10,15,65,67)),
assert(c325(fall_2010,kim,14,13,12,11,68,78)),
assert(setup(fall_2010,as1,15,0.1)),
assert(setup(fall_2010,as2,15,0.1)),
assert(setup(fall_2010,as3,15,0.1)),
assert(setup(fall_2010,as4,15,0.1)),
assert(setup(fall_2010,midterm,80,0.25)),
assert(setup(fall_2010,final,100,0.35)).
	
query1(Semester,Name,Total):-  
setup(Semester,as1,Max1,Percentage1),setup(Semester,as2,Max2,Percentage2),
setup(Semester,as3,Max3,Percentage3),
setup(Semester,as4,Max4,Percentage4),
setup(Semester,midterm,Max5,Percentage5),
setup(Semester,final,Max6,Percentage6),
c325(Semester,Name,A,B,C,D,E,F),
Total is ((A/Max1)*Percentage1+(B/Max2)*Percentage2+(C/Max3)*Percentage3+(D/Max4)*Percentage4+(E/Max5)*Percentage5+(F/Max6)*Percentage6)*100.

query2(Semester,L):-
findall([Name],cond(Name,Semester),L).
cond(N,S):-  
c325(S,N,_,_,_,_,E,F),
setup(S,midterm,Max5,_),
setup(S,final,Max6,_),
E/Max5 < F/Max6.

query3(Semester,Name,Type,Score):- c325(Semester,Name,_,_,_,_,_,_),!,print("record not found").
query3(Semester,Name,Type,Score):- print("record not found").

update(Semester,Name,Type,Score):- Type == as1,!,retract(c325(Semester,Name,_,A,B,C,D,E)),assert(c325(Semester,Name,Score,A,B,C,D,E)).
update(Semester,Name,Type,Score):- Type == as2,!,retract(c325(Semester,Name,A,_,B,C,D,E)),assert(c325(Semester,Name,A,Score,B,C,D,E)).
update(Semester,Name,Type,Score):- Type == as3,!,retract(c325(Semester,Name,A,B,_,C,D,E)),assert(c325(Semester,Name,A,_,Score,C,D,E)).
update(Semester,Name,Type,Score):- Type == as4,!,retract(c325(Semester,Name,A,B,C,_,D,E)),assert(c325(Semester,Name,A,B,C,Score,D,E)).
update(Semester,Name,Type,Score):- Type == midterm,!,retract(c325(Semester,Name,A,B,C,D,_,E)),assert(c325(Semester,Name,A,B,C,D,Score,E)).
update(Semester,Name,Type,Score):- Type == final,!,retract(c325(Semester,Name,A,B,C,D,E,_)),assert(c325(Semester,Name,A,B,C,D,E,Score)).



