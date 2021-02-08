flight(edirne,edremit).
flight(edremit,edirne).
flight(edremit,erzincan).
flight(erzincan,edremit).
flight(istanbul,izmir).
flight(izmir,istanbul).
flight(istanbul,antalya).
flight(antalya,istanbul).
flight(istanbul,gaziantep).
flight(gaziantep,istanbul).
flight(istanbul,ankara).
flight(ankara,istanbul).
flight(istanbul,van).
flight(van,istanbul).
flight(istanbul,rize).
flight(rize,istanbul).
flight(burdur,ısparta).
flight(ısparta,burdur).
flight(ısparta,izmir).
flight(izmir,ısparta).
flight(antalya,konya).
flight(konya,antalya).
flight(antalya,gaziantep).
flight(gaziantep,antalya).
flight(konya,ankara).
flight(ankara,konya).
flight(ankara,van).
flight(van,ankara).
flight(van,rize).
flight(rize,van).


%	That rule tries all the fact of flight. If fight from start city to intermediate city can occur
%	and that city is not in VisitList, searching is continue.
%	Recursive searching happens starting from start city.	
%	For example, if searching starts in A city and customer can fight from A to B and C cities,
%	The client goes from city a to city b and looks at the places she/he can go from there.
%	Then, client goes from city a to city c and looks at the places she/he can go from there.
%	This is how a recursive call search the cities. 
% 	Because of the recursive call, some cities shown more than once.


route(Start, End)			  :-	helper(Start, End, []).

helper(Start, End, VisitList) :-	flight(Start, Intermediate), 
									not(member(Intermediate, VisitList)),
									(End = Intermediate; helper(Intermediate, End, [Start | VisitList])).
					
