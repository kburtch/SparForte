procedure t is
  type r is record
       e : exception; -- not allowed in a record
  end record;
begin
  null;
end t;

