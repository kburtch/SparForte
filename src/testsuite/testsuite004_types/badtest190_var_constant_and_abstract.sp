# This should fail

procedure p is

  i : constant abstract integer := 1;
  pragma assumption( used, i );
begin
  null;
end p;

