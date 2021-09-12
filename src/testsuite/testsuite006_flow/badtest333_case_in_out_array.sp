# This should fail

i : limited integer := 1;
j : array( 1..1) of integer;

case in i out j is -- j is arrays
when 1 => 3;
when others => 4;
end case;

