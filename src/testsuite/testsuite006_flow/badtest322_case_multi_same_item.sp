# This should fail

i : constant integer := 1;

case i,i is -- duplicate identifier
when 1,1 => null;
when others => null;
end case;

