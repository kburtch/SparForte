procedure t is
  e : exception;
begin
  declare
    e : exception; -- SparForte does not permit two exceptions with same name
  begin
    null;
  end;
  null;
end t;

