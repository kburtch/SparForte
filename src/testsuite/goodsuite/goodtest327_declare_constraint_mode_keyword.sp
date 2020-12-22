file : limited integer := 1;
pragma assumption( used, file );
pragma declare_constraint( file, foo, bar ); -- existing identifier is permitted
