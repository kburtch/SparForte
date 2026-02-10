
js : json_string;
type atype is array(1..2) of integer;
a : atype;
aalias : atype renames a;

js := "[1,2]";
arrays.to_array(aalias, js);
pragma assert( a(1) = 1 );
pragma assert( a(2) = 2 );

