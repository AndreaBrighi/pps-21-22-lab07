% Ex 1
% 1.1
% search(Elem, List)

search(X,[X|_]).
search(X,[_|Xs]) :- search(X,Xs).

% 1.2
% search2(Elem, List)
% looks for two consecutive occurrences of Elem
search2(X,[X,X|_]). 
search2(X,[_|Xs]) :- search2(X,Xs).

% 1.3
% search_two(Elem,List)
% looks for two occurrences of Elem with any element in between!
search_two(X,[X,_,X|_]). 
search_two(X,[_|Xs]) :- search_two(X,Xs).

% 1.4
% search_anytwo(Elem,List)
% looks for any Elem that occurs two times, anywhere
search_anytwo(X,[X|T]) :- search(X, T).
search_anytwo(X,[_|T]) :- search_anytwo(X,T).

% Ex 2

% 2.1
% size(List , Size)
% Size will contain the number of elements in List
size([],0).
size([_|Xs],N) :- size(Xs,N2), N is N2 + 1.

% 2.2
% size(List ,Size)
% Size will contain the number of elements in List,
% written using notation zero , s(zero), s(s(zero))..
size_p([],zero).
size_p([_|Xs],s(N)) :- size_p(Xs,N).

% 2.3
% sum(List ,Sum)

sum([], 0).
sum([X| T], S):- sum(T, St), S is X + St.

% 2.4
% % average(List,Average)
% it uses average(List,Count,Sum,Average)

average(List,A) :- average(List,0,0,A). 
average([],C,S,A) :- A is S/C. 
average([X|Xs],C,S,A) :-
	C2 is C+1,
	S2 is S+X, average(Xs,C2,S2,A).

% 2.5
% max(List ,Max)
% Max is the biggest element in List
% Suppose the list has at least one element

greater(X, Y, Y):- X < Y.
greater(X, Y, X):- Y =< X.

max([X], X).
max([X| T], Max) :- max(T, Tmax), greater(X, Tmax, Max).

% 2.6
% max(List,Max,Min)
% Max is the biggest element in List
% Min is the smallest element in List
% Suppose the list has at least one element

lower(X, Y, Y):- X > Y.
lower(X, Y, X):- Y >= X.

max([X], X, X).
max([X| T], Max, Min) :- max(T, Tmax, Tmin), greater(X, Tmax, Max), lower(X, Tmin, Min).

% Ex 3
% 3.1
% same(List1 ,List2)
% are the two lists exactly the same?
same([],[]).
same([X|Xs],[X|Ys]) :- same(Xs,Ys).

% 3.2
% all_bigger(List1,List2)
% all elements in List1 are bigger than those in List2, 1 by 1
% example: all_bigger([10,20,30,40],[9,19,29,39]).
greater(X, Y):- Y < X.

all_bigger([X], [Y]):- greater(X, Y).
all_bigger([X|T1], [Y|T2]):-all_bigger(T1, T2).

% 3.3
% sublist(List1 ,List2)
% List1 should contain elements all also in List2 3 
% example: sublist([1,2],[5,3,2,1]).

sublist([X], Y):- search(X, Y).
sublist([X|T], Y):-search(X, Y), sublist(T, Y).


% Ex 4
% 4.1
% seq(N,List)
% example: seq(5,[0,0,0,0,0]).

seq(0,[]).
seq(N,[0|T]) :- N2 is N - 1, seq(N2,T).

% 4.2
% seqR(N,List)
% example: seqR(4,[4,3,2,1,0]).

seqR(0,[0]).
seqR(N, [N|T]):- R is N-1, seqR(R, T).

% 4.3
% seqR2(N,List)
% example: seqR2(4,[0,1,2,3,4]).

last(H, X, L):- append(H,[X], L).

seqR2(0, L):- last([], 0, L).
seqR2(N,L) :- last(H, N, L), N2 is N-1, seqR2(N2, H).

% 5

% l.last()
last(L, X):- last(_, X,L).
% l.map(_+1)
% op(I, R).
op(X, R):- R is X +1.

map([], []).
map([X|L], [Y|R]):-op(X, Y), map(L, R).

% l.filter(_>0)
pred(X):- X > 0.
filter([], []).
filter([X|L], [X|R]):- pred(X), filter(L, R).
filter([X|L], R):- not(pred(X)), filter(L, R).


% l.count(_>0)
count(L, N):-filter(L, R), size(R, N).

% l.find(_>0)
find(L, N) :- filter(L, R), search(N, R).

% l.dropRight(2)

dropRight([X| T], N, [X]) :- size(T, S), S =:= N.
dropRight([X| T], N, [X|R]):- size(T, S), S =\= N, dropRight(T, N, R).

% dropWhile(_>0)
dropWhile([],[]).
dropWhile([X| T], [X| T]) :- not(pred(X)).
dropWhile([X| T], R):- pred(X), dropWhile(T, R).

% partition(_>0)
partition([], [],[]).
partition([X|T], [X|G], L) :- pred(X), partition(T, G, L).
partition([X|T], G, [X|L]) :- not(pred(X)), partition(T, G, L).

% l.reversed()
reversed([X],[X]).
reversed([X| T], L):- reversed(T, Rt), append( Rt, [X], L).

% l.drop(2)
drop(L, N, L):- N =:=0.
drop([X|T], N, R):- N2 is N -1, drop(T, N2, R).

% l.take(2)
take(L, N, []):- N =:=0.
take([X|T], N, [X|R]):- N2 is N -1, take(T, N2, R).

% l.zip(l2)
zip([],[],[]).
zip([X1|T1], [X2|T2], [(X1,X2)|Tt]):-zip(T1, T2, Tt).




