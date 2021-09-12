# This should fail

type r is record
   i : integer;
end record;

i : limited integer := 1;
j : r;

case in i out j is -- j is record
when 1 => 3;
when others => 4;
end case;

