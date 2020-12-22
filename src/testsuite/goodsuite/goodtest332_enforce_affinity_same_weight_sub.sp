procedure t is
  pragma declare_affinity( file, foo );
  pragma affinity( foo, 0 );
  pragma affinity( foo, 0 ); -- weight unchanged
begin
  null;
end t;

