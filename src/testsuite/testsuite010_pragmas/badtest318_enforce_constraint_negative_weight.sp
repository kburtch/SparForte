# This should fail

pragma declare_constraint( file, development_team, team_a, 2 );
pragma declare_constraint( file, development_team, team_b );
pragma constraint( development_team, team_a,  1 );
pragma constraint( development_team, team_a, -1 ); -- negative weight

