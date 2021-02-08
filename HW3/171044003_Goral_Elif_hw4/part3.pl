
when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

%	---------------------------------------------------------------------------------------
%	predicate “schedule(S,P,T)” that associates a student to a place and time of class. See the example query and its result.
%	when(className, Time)
%	where(className, Place)
%	enroll(student, className)

shedule(S,P,T) :- 	enroll(S,C), 
					where(C,P),
					when(C,T).

%	---------------------------------------------------------------------------------------
%	predicate “usage(P,T)” that gives the usage times of a classroom. See the example query and its result.
usage(P,T)	:-	where(C,P),
				when(C,T).

%	---------------------------------------------------------------------------------------
%	X and Y are classNames. 
%	predicate “conflict(X,Y)” that gives true if X and Y conflicts due to classroom or time.

conflict(X,Y) :- (when(X,Time1), when(Y,Time2), Time1==Time2) ;
				 (where(X,Place1), where(Y,Place2), Place1==Place2).

%	---------------------------------------------------------------------------------------
%	X and Y are students.
%	predicate “meet(X,Y)” that gives true 
%	if student X and student Y are present in the same classroom at the same time.
%	For this purpose, I control the same classes. If  students are in same class which means
%	They are in same classroom at the same time.

meet(X,Y):-		enroll(X,ClassName1), 
				enroll(Y,ClassName2), 
				ClassName1==ClassName2, !.
