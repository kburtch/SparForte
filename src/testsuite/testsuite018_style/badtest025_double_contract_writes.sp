procedure t is

  i : integer := 5;

  exc : exception;

  subtype risky_integer1 is integer
     affirm
       i := 1; -- double-write to global
       raise exc when risky_integer1 < 0;
     end affirm;

  subtype risky_integer2 is integer
     affirm
       i := 2; -- double-write to global
       raise exc when risky_integer2 < 0;
     end affirm;

begin
  ? risky_integer1(1) + risky_integer2(2);
end t;
