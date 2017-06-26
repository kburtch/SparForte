# This should fail

pragma ada_95;
subtype t is abstract integer; --not allowed
type s is new t;
a : s := 1;
put_line( a );

