procedure t is

case procedure abstract_cproc( i : integer; j : out integer ) is abstract
   when 1 => 2;
   when others => 0;
end abstract_cproc;

  res : constant integer := 0;
begin
  abstract_cproc( 1, res ); -- ERROR: should not run
  ? res;
end t;

