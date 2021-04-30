-- This should fail

procedure t is
   x1   : integer;
   xone : constant integer := 1;
begin
   x1 := xone;
   ? x1;
end t;
