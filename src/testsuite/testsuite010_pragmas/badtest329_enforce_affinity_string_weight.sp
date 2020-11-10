# This should fail

pragma declare_affinity( file, pod1, 1 );
pragma affinity( pod1, "weight" ); -- should be float

