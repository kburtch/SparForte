procedure t is
  e : exception "foo" use 1; -- missing with
begin
  null;
end t;

