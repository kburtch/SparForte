procedure t is
  pragma restriction( no_risky_side_effects );
  b : boolean := false;

  function risky1 return integer is
  begin
    b; -- double-write to global by function
    return 1;
  end risky1;

  function risky2 return integer is
  begin
    b; -- double-write to global by function
    return 1;
  end risky2;

begin
  ? risky1;
  ? risky2;
  ? b;
end t;
