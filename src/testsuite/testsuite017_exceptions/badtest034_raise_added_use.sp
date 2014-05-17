procedure t is
  e : exception;
begin
  raise e with "foo" use 1; -- use not allowed
  exception when others =>
  null;
end t;

