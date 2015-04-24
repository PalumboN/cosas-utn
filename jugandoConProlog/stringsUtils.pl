camelCaseToSnakeCase(StringAsAtom, SnakeCaseAsAtom) :-
	name(StringAsAtom, String),
	convertCamelCaseToSnakeCase(String, SnakeCase),
	name(SnakeCaseAsAtom, SnakeCase).
	
camelCaseToSnakeCase(StringAsAtom, SnakeCaseAsAtom) :-
	name(SnakeCaseAsAtom, SnakeCase),
	convertCamelCaseToSnakeCase(String, SnakeCase),
	name(StringAsAtom, String).
	
convertCamelCaseToSnakeCase([], []).
	
convertCamelCaseToSnakeCase([MayusAscii | String], [95, MinusAscii | Result]) :-
	between(65, 90, MayusAscii),
	plus(32, MayusAscii, MinusAscii),
	convertCamelCaseToSnakeCase(String, Result).
	
convertCamelCaseToSnakeCase([MinusAscii | String], [MinusAscii | Result]) :-	
	between(97, 122, MinusAscii),
	convertCamelCaseToSnakeCase(String, Result).
	
convertCamelCaseToSnakeCase([NumberAscii | String], Result) :-	
	between(48, 57, NumberAscii),
	append("_", [NumberAscii], WithSpace),
	convertCamelCaseToSnakeCase(String, SubResult),
	append(WithSpace, SubResult, Result).