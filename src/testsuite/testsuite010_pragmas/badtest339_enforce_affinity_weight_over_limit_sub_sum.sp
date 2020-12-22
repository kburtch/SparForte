# This should fail

procedure t is
pragma declare_affinity( subprogram, development_team, 1 );
pragma affinity( development_team, 1 );
pragma affinity( development_team, 1 ); -- over the limit of 1
begin
  null;
end t;

