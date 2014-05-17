procedure t is
  e : exception;
begin
  raise e "foo"; -- missing with
  exception when others =>
  null;
end t;

