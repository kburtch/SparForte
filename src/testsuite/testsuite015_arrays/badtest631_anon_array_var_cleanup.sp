procedure p is
  anon_array : array(1..2) of integer := (1);
  -- this should cause anon_array to be destroyed
begin
  ? anon_array(1);
end p;

