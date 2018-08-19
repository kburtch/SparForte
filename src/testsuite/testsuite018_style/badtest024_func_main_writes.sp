procedure t is
  i : integer := 5;

  function risky1 return integer is
  begin
    i := 10; -- double-write to global by function
    return 1;
  end risky1;

begin
  i := 10; --double-write to global by main
  ? risky1;
end t;
