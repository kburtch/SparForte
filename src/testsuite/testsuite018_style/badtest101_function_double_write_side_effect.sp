procedure t is

  x : integer;

  function writes_x return integer is
  begin
     x := 0;
     return x;
  end writes_x;

begin
  x := 3;
  x := writes_x; -- double write side-effect
end t;

