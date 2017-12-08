procedure ren is
  type intarray is constant array( 1..3 ) of integer;
  a : intarray := (1, 2, 3);
  b : limited intarray renames a;
  i : integer;
begin
  i := b(1); -- this should be limited (no factor)
end ren;

-- vim: ft=spar

