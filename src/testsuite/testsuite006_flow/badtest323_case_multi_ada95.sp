# This should fail

pragma ada_95;

i : constant integer := 1;
j : constant integer := 1;

case i,j is -- multiple idents not allowed
when 1,1 => null;
when others => null;
end case;

? i;
? j;

