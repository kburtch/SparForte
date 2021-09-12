# This should fail

i : limited integer := 1;
j : exception;

case in i out j is -- j is exception
when 1 => 3;
when others => 4;
end case;

