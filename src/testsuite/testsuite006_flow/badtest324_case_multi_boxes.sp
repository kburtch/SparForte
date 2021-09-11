# This should fail

i : constant integer := 1;
j : constant integer := 1;

case i,j is
when <>,<> => null; -- not allowed
when others => null;
end case;

? i;
? j;

