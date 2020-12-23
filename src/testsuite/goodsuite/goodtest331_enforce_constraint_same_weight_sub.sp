procedure t is
  pragma declare_constraint( subprogram, foo, bar );
  pragma constraint( foo, bar, 0 );
  pragma constraint( foo, bar, 0 ); -- weight unchanged
begin
  null;
end t;
