procedure t is
  e : exception with "foo" use ; -- missing use value
begin
  null;
end t;

