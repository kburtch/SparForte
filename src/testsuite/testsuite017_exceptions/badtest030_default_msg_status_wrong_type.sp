procedure t is
  e : exception with "foo" use "bar"; -- wrong type
begin
  null;
end t;

