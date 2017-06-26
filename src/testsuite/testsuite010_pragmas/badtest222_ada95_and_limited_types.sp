# This should fail

pragma ada_95;
subtype t is limited integer; --not allowed
a : t := 1;
put_line( a );

