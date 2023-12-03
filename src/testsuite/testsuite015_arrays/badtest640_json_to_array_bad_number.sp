js: json_string;
a : array(1..2) of integer;

js := "[ 1, true ]";
arrays.to_array( a, js ); -- true is not an integer

