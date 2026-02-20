meta inches is new meta;

js : json_string;
type atype is array(1..2) of integer;
a : atype;
aalias : atype renames a;

js := "[1,2]" tagged inches;
arrays.to_array(aalias, js);
pragma assert( not tags.has_unit( a(1) ) );
pragma assert( not tags.has_unit( a(2) ) );

