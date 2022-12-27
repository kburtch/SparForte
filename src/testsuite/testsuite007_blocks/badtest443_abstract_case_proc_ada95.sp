procedure t is

pragma ada_95;

case procedure cproc( i : integer; j : out integer ) is -- ERROR
   when 1 => 2;
   when others => 0;
end cproc;

res : integer := 1;
begin
  cproc( 1, res );
  ? res;
end t;

