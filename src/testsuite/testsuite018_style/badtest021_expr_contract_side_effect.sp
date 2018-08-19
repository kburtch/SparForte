procedure t is

  i : integer := 5;

  exc : exception;

  subtype risky_integer is integer
     affirm
        i := 0;
        raise exc when risky_integer<0;
     end affirm;

  risky : risky_integer := 10;

begin
  ? risky * risky_integer(15) + i; -- changing i during an expression
end t;
