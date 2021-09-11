# This should fail

i : constant integer := 1;

case i, is -- missing case identifier
when 1, 1 => null;
when others => null;
end case;

? i;

