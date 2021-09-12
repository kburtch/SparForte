# This should fail

i : limited integer := 1;
j : constant integer := 0;

case in i out j is -- j is constant
when 1 => 3;
when others => 4;
end case;

