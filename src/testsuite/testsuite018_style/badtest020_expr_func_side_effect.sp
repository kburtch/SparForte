procedure t is
  i : integer := 5;

  function risky return integer is
  begin
    i := 10;
    return 1;
  end risky;

begin
  ? i * risky + i; -- changing i during an expression
end t;
