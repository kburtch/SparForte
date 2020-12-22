# This should fail

pragma declare_constraint( file, development_team, team_a, 1 );
pragma constraint( development_team, team_a, 1 );
pragma constraint( development_team, team_a, 1 ); -- over the limit of 1

