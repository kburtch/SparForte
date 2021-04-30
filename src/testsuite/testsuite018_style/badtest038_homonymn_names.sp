-- This should fail

procedure t is
  x1   : integer;
  xOne : constant integer := 1;
begin
  x1 := xOne;
  ? x1;
end t;

