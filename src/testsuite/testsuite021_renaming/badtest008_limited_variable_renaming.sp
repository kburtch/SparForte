procedure t4 is
  i : limited integer := 1;
  j : integer renames i; -- fail, not limtied
begin
  j := 1;
end t4;

