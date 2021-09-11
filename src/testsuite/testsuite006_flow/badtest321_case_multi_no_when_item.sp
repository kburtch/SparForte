# This should fail

i : constant integer := 1;
j : constant integer := 1;

case i,j is -- missing case identifier
when 1, => null;
when others => null;
end case;

? i;
? j;
