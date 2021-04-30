-- This should fail

procedure t is
  xOne : constant integer := 1;
  x1   : integer;
begin
  x1 := xOne;
  ? x1;
end t;
