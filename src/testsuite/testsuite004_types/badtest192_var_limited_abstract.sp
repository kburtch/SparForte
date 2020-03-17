# This should fail

procedure p is

  i : limited abstract integer := 1;
  pragma assumption( used, i );
begin
  null;
end p;

