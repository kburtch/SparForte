procedure t is
pragma declare_constraint( subprogram, foo, bar );
pragma declare_constraint( subprogram, foo, baz );

procedure inner1 is
  pragma constraint( foo, bar );
begin
  null;
end inner1;

procedure inner2 is
  pragma constraint( foo, baz );
begin
  null;
end inner2;

begin
  inner1;
  inner2;
end t;

