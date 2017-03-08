t : dynamic_hash_tables.table( string );
dynamic_hash_tables.set( t, "foo", "bar" );
dynamic_hash_tables.set( t, "foo2", "bar2" );
s : string;
eof : boolean;
dynamic_hash_tables.get_first( t, s, eof );
pragma ada_95;
dynamic_hash_tables.get_next( t, s, eof ); -- not allowed

