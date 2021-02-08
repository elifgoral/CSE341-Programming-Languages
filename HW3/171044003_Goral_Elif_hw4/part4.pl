
% 	predicate “element(E,S)” that returns true if E is in S.
%	Firstly I control the position of element. If element exist in head of list,
% 	It returns true, otherwise it continues to next statement.
%	Which looks rest of the list with recursive calls
element(E, [E|_]).
element(E, [_|S]):- element(E, S).


%	predicate “union(S1,S2,S3)” that returns true if S3 is the union of S1 and S2.
%	Firstly I find the union of S1 and S2, Then I control the union and S3 are equivalent or not. 
union(S1,S2,S3) :- unionHelper(S1,S2,X), equivalent(X,S3).
unionHelper([], S2, S2).
unionHelper([E|S1], S2, S3) :- element(E, S2), !, unionHelper(S1, S2, S3). 
unionHelper([E|S1], S2, [E|S3]) :- unionHelper(S1, S2, S3).


%	predicate “intersect(S1,S2,S3)” that returns true if S3 is the intersection of of S1 and S2.
%	Firstly I find the intersection of S1 and S2, Then I control the intersection and S3 are equivalent or not. 
intersect(S1,S2,S3) :- intersectHelper(S1,S2,X), equivalent(X,S3).
intersectHelper([], _, []).
intersectHelper([E|S1], S2, [E|S3]):- element(E, S2), !, intersectHelper(S1, S2, S3).
intersectHelper([_|S1], S2, S3):- intersectHelper(S1, S2, S3).


%	predicate “equivalent(S1,S2)” that returns true if S1 and S2 are equivalent sets.
%	If s1 and s2 lists are empty, returns true. otherwise,
%	I control the elements of s1 list in s2 list respectively. Then I do same process for s2 list.
%	I control the elements of s2 list in s1 list respectively.
equivalent(S1, S2) :- equivalentHelper(S1,S2), equivalentHelper(S2,S1).
equivalentHelper([],_).
equivalentHelper([E|S1],S2):- element(E,S2), equivalentHelper(S1,S2).