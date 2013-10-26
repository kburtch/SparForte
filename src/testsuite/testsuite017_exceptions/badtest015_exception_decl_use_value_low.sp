procedure t is
  e : exception with "foo" use -1; -- value too low
begin
  null;
end t;

