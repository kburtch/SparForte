# This should fail

subtype t is abstract limited integer; --cant limit an abstract
a : t := 1;


