t : dynamic_hash_tables.table( string );
dynamic_hash_tables.set( t, "foo", "bar" );
i : integer := 1;
s : string;
eof : boolean;
dynamic_hash_tables.get_first( t, s, eof );
dynamic_hash_tables.get_next( t, i, eof ); -- should be s

