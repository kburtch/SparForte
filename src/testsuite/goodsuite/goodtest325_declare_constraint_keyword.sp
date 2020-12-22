i : limited integer := 1;
pragma assumption( used, i );
pragma declare_constraint( file, i, bar ); -- existing identifier is permitted
