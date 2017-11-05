procedure t4 is
  type array_type is array(1..3) of integer;
  a : limited array_type := (1,2,3);
  b : integer renames a(1); -- fail, not limtied
begin
  b := 1;
end t4;

