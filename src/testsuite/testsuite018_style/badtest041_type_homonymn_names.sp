-- This should fail

procedure t is
  x1   : integer;
  type xOne is new integer;
  x : constant xOne := 1;
begin
  x1 := x;
  ? x1;
end t;

