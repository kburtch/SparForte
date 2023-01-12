procedure t is

case procedure cproc( i : integer; j : out integer ) is
   when 1 2; -- ERROR
   when others => 0;
end cproc;

res : integer := 1;
begin
  cproc( 1, res );
  ? res;
end t;

