js : json_string := "[ true, flase ]";
type arrtype is array(1..2) of boolean;

a : arrtype;

arrays.to_array( a, js ); -- bool values is wrong
