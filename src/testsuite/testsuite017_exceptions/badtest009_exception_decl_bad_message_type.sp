procedure t is
  e : exception with false use 1; -- bad message type
begin
  null;
end t;

