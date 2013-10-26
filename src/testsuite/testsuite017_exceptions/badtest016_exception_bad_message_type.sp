procedure t is
  e : exception;
begin
  raise e with false; -- wrong message type
end t;

