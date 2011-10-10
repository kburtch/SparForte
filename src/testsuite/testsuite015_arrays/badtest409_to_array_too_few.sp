type int_array is array(1..2) of integer;
a : int_array;
js : json_string := "[1]";
arrays.to_array( a, js ); -- too few in json string

