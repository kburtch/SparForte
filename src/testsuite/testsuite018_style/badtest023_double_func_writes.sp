procedure t is
  pragma restriction( no_risky_side_effects );
  i : integer := 5;

  function risky1 return integer is
  begin
    i := 10; -- double-write to global by function
    return 1;
  end risky1;

  function risky2 return integer is
  begin
    i := 10; -- double-write to global by function
    return 1;
  end risky2;

begin
  ? risky1;
  ? risky2;
end t;
