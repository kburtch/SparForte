js: json_string;
a : array(1..3) of integer;

js := "[ 1,,3 ]";
arrays.to_array( a, js ); -- item 2 is missing

