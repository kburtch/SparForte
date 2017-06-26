# this should fail

type t1 is new integer;
type t2 is new t1;

i : t2;

i := 5;

-- t1 not used to define a variable

