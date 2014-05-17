procedure t is
  e : exception;
begin
  raise e with 1234; -- wrong type
  exception when others =>
  null;
end t;

