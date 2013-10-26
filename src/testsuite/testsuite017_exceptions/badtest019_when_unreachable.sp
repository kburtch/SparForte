procedure t is
  e : exception;
begin
  null;
  exception when e =>
    raise;
    null; -- unreachable
end t;

