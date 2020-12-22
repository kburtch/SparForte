# This should fail

pragma declare_affinity( file, pod, 2 );
pragma affinity( pod,  1 );
pragma affinity( pod, -1 ); -- negative weight

