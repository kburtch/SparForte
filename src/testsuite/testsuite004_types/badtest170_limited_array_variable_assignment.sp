procedure p is
  type array_type is array(1..3) of integer;
  a : limited array_type;
begin
  a(1) := 1; -- should fail
end p;

