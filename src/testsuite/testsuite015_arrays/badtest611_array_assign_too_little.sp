type array_type is array(1..2) of integer;
a : array_type := (1); -- too much data
a(1):=1;
