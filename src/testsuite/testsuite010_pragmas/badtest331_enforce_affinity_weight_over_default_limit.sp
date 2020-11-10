# This should fail

pragma declare_affinity( file, pod1 );
pragma affinity( pod1, 1 ); -- over the limit of 0

