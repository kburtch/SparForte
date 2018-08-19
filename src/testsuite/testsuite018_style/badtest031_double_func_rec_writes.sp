procedure t is
  type risky_record is record
    i : integer;
    j : integer;
  end record;
  r : risky_record := (1,2);

  function risky1 return integer is
  begin
    r.i := 10; -- double-write to global by function
    return 1;
  end risky1;

  function risky2 return integer is
  begin
    r.j := 10; -- double-write to global by function
    return 1;
  end risky2;

begin
  ? risky1;
  ? risky2;
end t;
