# This should fail

i : limited integer := 1;
j : limited command := "/bin/false";

case in i out j is -- j is command
when 1 => 3;
when others => 4;
end case;

