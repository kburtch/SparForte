procedure t is
  i : integer := 5;

  exc : exception;

  subtype risky_integer is integer
     affirm
       i := 1; -- double-write to global
       raise exc when risky_integer < 0;
     end affirm;

begin
  i := 10; --double-write to global by main
  ? risky_integer(1);
end t;
