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

distance(edirne,edremit,235).
distance(edremit,edirne,235).
distance(edremit,erzincan,1066).
distance(erzincan,edremit,1066).
distance(burdur,ısparta,25).
distance(ısparta,burdur,25).
distance(ısparta,izmir,309).
distance(izmir,ısparta,309).
distance(izmir,istanbul,329).
distance(istanbul,izmir,329).
distance(antalya,istanbul,483).
distance(istanbul,antalya,483).
distance(istanbul,gaziantep,847).
distance(gaziantep,istanbul,847).
distance(ankara,istanbul,352).
distance(istanbul,ankara,352).
distance(istanbul,van,1262).
distance(van,istanbul,1262).
distance(rize,istanbul,968).
distance(istanbul,rize,968).
distance(konya,antalya,192).
distance(antalya,konya,192).
distance(antalya,gaziantep,592).
distance(gaziantep,antalya,592).
distance(konya,ankara,227).
distance(ankara,konya,227).
distance(ankara,van,920).
distance(van,ankara,920).
distance(van,rize,373).
distance(rize,van,373).


%	Firstly I control the direct fight from start city to end city.
%	Then calculate the distance.
sroute(StartCity,EndCity,Distance) :- 	flight(StartCity,EndCity), 
										distance(StartCity,EndCity,Distance).
					

%	I control the route which has one extra flight in between start city and end city.
sroute(StartCity,EndCity,Distance) :- 	flight(StartCity,IntermediateCity), 
										flight(IntermediateCity,EndCity), 
										distance(StartCity,IntermediateCity,Distance1),
										distance(IntermediateCity,EndCity,Distance2),
										Distance is Distance1+Distance2.
										

%	I control the route which has two extra flight in between start city and end city.
%	I did not check any more extra flights. I checked the situation of two flights between Istanbul and Burdur. No more extra flights is needed on flights between other cities.
sroute(StartCity,EndCity,Distance) :- 	flight(StartCity,IntermediateCity), 
										flight(IntermediateCity,IntermediateCity2),
										flight(IntermediateCity2,EndCity), 
										distance(StartCity,IntermediateCity,Distance1),
										distance(IntermediateCity,IntermediateCity2,Distance2),
										distance(IntermediateCity2,EndCity,Distance3),
										Distance is Distance1+Distance2+Distance3.
