procedure t is
  type risky_record is record
     i : integer;
     j : integer;
  end record;
  r : risky_record := (1,2);

  function risky return integer is
  begin
    r.j := 10;
    return 1;
  end risky;

begin
  ? r.i * risky + r.i; -- changing array during an expression, any field
end t;
