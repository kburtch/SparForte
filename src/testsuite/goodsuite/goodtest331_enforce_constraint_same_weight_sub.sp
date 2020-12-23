procedure t is
  pragma declare_constraint( subprogram, foo, bar, 2 );
  pragma constraint( foo, bar, 1 );
  pragma constraint( foo, bar, 1 );
begin
  null;
end t;
