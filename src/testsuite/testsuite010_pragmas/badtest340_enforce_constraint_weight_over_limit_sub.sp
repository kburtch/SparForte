# This should fail
procedure t is
  pragma declare_constraint( subprogram, development_team, team_a, 1 );
  pragma constraint( development_team, team_a, 2 ); -- over the limit of 1
begin
  null;
end t;

