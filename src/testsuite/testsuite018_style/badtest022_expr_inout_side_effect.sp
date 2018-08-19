procedure t is
  i : integer := 5;

  function risky( p : in out integer ) return integer is
  begin
    p := 10;
    return 1;
  end risky;

begin
  ? i * risky(i) + i; -- changing i during an expression
end t;
