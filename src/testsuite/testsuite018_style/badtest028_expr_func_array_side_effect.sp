procedure t is
  type risky_array is array(1..2) of integer;
  i : risky_array := (1,2);

  function risky return integer is
  begin
    i(2) := 10;
    return 1;
  end risky;

begin
  ? i(1) * risky + i(1); -- changing array during an expression, any element
end t;
