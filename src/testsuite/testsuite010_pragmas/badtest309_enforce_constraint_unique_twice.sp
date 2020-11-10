# This should fail

pragma declare_constraint( unique, development_team, team_a );
pragma constraint( development_team, team_a );
pragma constraint( development_team, team_a ); -- unique constraint twice

