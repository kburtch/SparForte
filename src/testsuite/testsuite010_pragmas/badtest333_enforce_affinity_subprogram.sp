# This should fail
procedure t is
pragma declare_affinity( subprogram, pod1 );

procedure inner is
   pragma affinity( pod1 );
begin
  null;
end inner;

procedure inner2 is
   pragma affinity( pod1 ); -- need to be in inner
begin
  null;
end inner2;

begin
  inner;
  inner2;
end t;

