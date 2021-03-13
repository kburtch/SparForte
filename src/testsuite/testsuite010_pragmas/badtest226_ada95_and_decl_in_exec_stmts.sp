# This should fail

procedure t is
  pragma ada_95;
begin
   x : integer := 1; -- declaration in exec stmts part
   ? x;
end t;


