# This should fail

i : constant integer := 1;
j : constant integer := 1;

case i,j is
when 1 => null; -- too few
when others => null;
end case;

