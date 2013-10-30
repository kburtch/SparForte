procedure t is
  e : exception;
  ut : universal_typeless := e; -- exceptions cannot be assigned
begin
  ut := ut;
end t;

