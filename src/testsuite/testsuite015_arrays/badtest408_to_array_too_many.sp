type int_array is array(1..1) of integer;
a : int_array;
js : json_string := "[1,2]";
arrays.to_array( a, js ); -- too many in json string

