procedure p is
  type arr_type is array(1..1) of integer;
  anon_array : array(1..1) of arr_type; -- not currently supported
begin
  ? anon_array(1);
end p;

