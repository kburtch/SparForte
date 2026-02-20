meta policy fakepolicy is new meta;

js : json_string;
type atype is array(1..2) of integer;
a : atype;

a(1) := 1 tagged policy fakepolicy;
a(2) := 2 tagged policy fakepolicy;
arrays.to_json(js, a);
pragma assert( not tags.has_policies( js ) );

