# This should fail

procedure p is
  pragma ada_95;

  type i is new limited integer; -- not allowed

  j : i := 1;
  pragma assumption( used, j );
begin
  null;
end p;

