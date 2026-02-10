
js : json_string;
type atype is array(1..2) of integer;
a : atype;
aalias : atype renames a;

a(1) := 1;
a(2) := 2;
js := "[1,2]";
arrays.to_json(js, aalias);
pragma assert( js = "[1,2]" );

