# This should fail

procedure p is

  i : constant limited integer := 1;
  pragma assumption( used, i );
begin
  null;
end p;

