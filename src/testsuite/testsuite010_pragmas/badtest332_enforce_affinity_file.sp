# This should fail

procedure t is
pragma declare_affinity( file, pod1 );

with separate "_badtest332_separate.sp";
pragma affinity( pod1 );

begin
  null;
end t;

