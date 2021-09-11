# This should fail

pragma ada_95;

i : constant integer := 1;
j : integer;

case in i out j is -- not allowed with ada95
when 1 => 3;
when others => 4;
end case;

? i;
? j;

