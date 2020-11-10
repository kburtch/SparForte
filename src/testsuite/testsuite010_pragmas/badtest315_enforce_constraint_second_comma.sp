# This should fail

pragma declare_constraint( file, development_team, team_a, 100 );
pragma declare_constraint( file, development_team, team_b, 100 );
pragma constraint( development_team, team_a 1 ); -- second comma missing

