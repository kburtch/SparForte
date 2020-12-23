procedure t is
  pragma declare_affinity( subprogram, foo, 2 );
  pragma affinity( foo, 1 );
  pragma affinity( foo, 1 );
begin
  null;
end t;

