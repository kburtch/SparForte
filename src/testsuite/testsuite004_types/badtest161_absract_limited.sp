# This should fail

type t is new abstract limited integer; --cant limit an abstract
a : t := 1;


