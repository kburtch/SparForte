procedure t is
  ut : universal_typeless; -- exceptions cannot be assigned
  e : exception := ut;
begin
  ut := ut;
end t;

