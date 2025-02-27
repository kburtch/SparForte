# This should fail

c : constant integer;
c : constant integer := "foobar"; -- foobar is string
? c;

