# This should fail

pragma declare_affinity( file, development_team, 1 );
pragma affinity( development_team, 1 );
pragma affinity( development_team, 1 ); -- over the limit of 1

