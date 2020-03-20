procedure p is
  anon_array : array(2..1) of integer; -- error: bounds reversed
begin
  ? anon_array(1);
end p;

