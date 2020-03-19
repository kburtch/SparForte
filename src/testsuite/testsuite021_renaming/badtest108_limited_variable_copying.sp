procedure t4 is
  i : limited integer := 1;
  j : integer copies i; -- fail, limited cannot be copied
  pragma assumption( used, j );
begin
  null;
end t4;

