t : dynamic_hash_tables.table( string );
dynamic_hash_tables.set( t, "foo", "bar" );
s : string;
eof : boolean;
pragma ada_95;
dynamic_hash_tables.get_first( t, s, eof ); -- not allowed

