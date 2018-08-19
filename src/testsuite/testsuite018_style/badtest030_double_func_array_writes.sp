procedure t is
  type risky_array is array(1..2) of integer;
  i : risky_array := (1,2);

  function risky1 return integer is
  begin
    i(1) := 10; -- double-write to global by function
    return 1;
  end risky1;

  function risky2 return integer is
  begin
    i(2) := 10; -- double-write to global by function
    return 1;
  end risky2;

begin
  ? risky1;
  ? risky2;
end t;
