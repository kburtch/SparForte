# This should fail

i : constant integer := 1;
j : constant integer := 1;

case i,j is
when 1,2 => null; -- no when
when => null;
end case;

? i;
? j;

