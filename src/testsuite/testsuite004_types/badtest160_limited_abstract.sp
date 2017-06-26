# This should fail

type t is new limited abstract integer; --cant limit an abstract
a : t := 1;


