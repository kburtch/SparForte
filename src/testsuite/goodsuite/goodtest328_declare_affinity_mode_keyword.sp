file : limited integer := 1;
pragma assumption( used, file );
pragma declare_affinity( file, foo ); -- existing identifier is permitted

