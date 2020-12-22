# This should fail
procedure t is
  pragma declare_affinity( subprogram, pod, 1 );
  pragma affinity( pod, 2 ); -- over the limit of 1
begin
  null;
end t;

