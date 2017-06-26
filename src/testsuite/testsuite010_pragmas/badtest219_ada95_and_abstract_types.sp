# This should fail

pragma ada_95;
type t is new abstract integer; --not allowed
type s is new t;
a : s := 1;
put_line( a );

