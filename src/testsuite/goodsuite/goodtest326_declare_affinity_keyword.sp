i : limited integer := 1;
pragma assumption( used, i );
pragma declare_affinity( file, i ); -- existing identifier is permitted

