# This should fail

pragma declare_affinity( file, pod1, 1 );
pragma affinity( pod1, 2 ); -- over the limit of 1

