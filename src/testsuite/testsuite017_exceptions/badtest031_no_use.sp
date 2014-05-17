procedure t is
  e : exception with "foo" 1; -- missing use
begin
  null;
end t;

