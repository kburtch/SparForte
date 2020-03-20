procedure p is
  x : integer;
  anon_array : array(x..1) of integer; -- error: x undefined
begin
  ? anon_array(1);
end p;

