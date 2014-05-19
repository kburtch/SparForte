procedure t is
  pragma ada_95;
  e : exception with "foo" use 1; -- not allowed with pragma ada_95
begin
  raise e;
  exception when e =>
  null;
end t;

