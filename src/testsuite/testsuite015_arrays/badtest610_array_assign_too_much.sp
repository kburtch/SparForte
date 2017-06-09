type array_type is array(1..1) of integer;
a : array_type := (1,2); -- too much data
a(1):=1;
