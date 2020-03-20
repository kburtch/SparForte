procedure p is
  x : integer;
  anon_array : array(1..x) of integer; -- error: x undefined
begin
  ? anon_array(1);
end p;

