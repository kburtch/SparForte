procedure t is
  pragma declare_affinity( subprogram, foo );
  pragma affinity( foo, 0 );
  pragma affinity( foo, 0 ); -- weight unchanged
begin
  null;
end t;

