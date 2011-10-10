type str_array is array(1..1) of string;
a : str_array;
js : json_string := "[1]";
arrays.to_array( a, js ); -- string but integer supplied

