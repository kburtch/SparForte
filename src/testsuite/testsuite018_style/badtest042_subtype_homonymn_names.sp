-- This should fail

procedure t is
  x1   : integer;
  subtype xOne is integer;
  x : constant xOne := 1; -- soundalike
begin
  x1 := x;
  ? x1;
end t;

