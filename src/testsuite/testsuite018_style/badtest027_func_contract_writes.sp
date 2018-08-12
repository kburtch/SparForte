procedure t is
  pragma restriction( no_risky_side_effects );
  i : integer := 5;

  exc : exception;

  subtype risky_integer is integer
     affirm
       i := 1; -- double-write to global
       raise exc when risky_integer < 0;
     end affirm;

  function risky return integer is
  begin
     i := 1; -- double-write to global
     return 0;
  end risky;

begin
  ? risky_integer(1) + risky;
end t;
