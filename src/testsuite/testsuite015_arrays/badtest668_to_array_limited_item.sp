
type li is new limited integer;

a : array(1..2) of li;
js : json_string := "[1,2]";

arrays.to_array( a, js ); -- cannot change limiteds

