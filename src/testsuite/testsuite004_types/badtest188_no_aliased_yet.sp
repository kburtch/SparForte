# This should fail

procedure p is

  i : aliased integer := 1;
begin
  ? i;
end p;

