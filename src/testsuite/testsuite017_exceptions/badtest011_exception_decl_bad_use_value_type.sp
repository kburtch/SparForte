procedure t is
  e : exception with "foo" use false; -- bad use value type
begin
  null;
end t;

