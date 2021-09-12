# This should fail

i : constant integer := 1;
j : constant integer := 1;

case i,j is
 1,2,3 => null; -- no when
when others => null;
end case;

