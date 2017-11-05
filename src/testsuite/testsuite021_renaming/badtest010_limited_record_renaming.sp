procedure t4 is
  type rec_type is record
       i : integer;
  end record;
  a : limited rec_type;
  b : integer renames a.i; -- fail, not limited
begin
  b := 1;
end t4;

