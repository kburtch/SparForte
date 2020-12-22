# This should fail

pragma declare_constraint( unique, development_team, team_a, 1 );
pragma constraint( development_team, team_a, 2 ); -- over the limit of 1

