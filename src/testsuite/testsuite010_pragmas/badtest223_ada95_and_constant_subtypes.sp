# This should fail

pragma ada_95;
subtype t is constant integer; --not allowed
a : t := 1;
put_line( a );

