-- This should fail

procedure t is
  xone : constant integer := 1;
  x1   : integer;
begin
  x1 := xone;
  ? x1;
end t;
