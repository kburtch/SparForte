# This should fail

procedure t is
  pragma restriction( no_declarations_in_executable_statements );
begin
   x : integer := 1; -- declaration in exec stmts part
   ? x;
end t;


