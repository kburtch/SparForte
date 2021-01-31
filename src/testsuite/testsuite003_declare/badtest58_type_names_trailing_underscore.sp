pragma ada_95;

type s_ is new string;
s : constant s_ := "foobar"; -- should not end with underscore
put_line( s );

