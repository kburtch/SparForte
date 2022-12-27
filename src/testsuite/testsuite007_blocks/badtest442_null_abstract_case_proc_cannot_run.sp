procedure t is

case procedure abstract_cproc( i : integer; j : out integer ) is null abstract;

  res : constant integer := 0;
begin
  abstract_cproc( 1, res ); -- ERROR: should not run
  ? res;
end t;

