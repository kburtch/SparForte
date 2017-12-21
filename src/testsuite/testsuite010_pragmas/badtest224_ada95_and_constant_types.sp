# This should fail

pragma ada_95;
type t is new constant integer; --not allowed
a : t := 1;
put_line( a );

