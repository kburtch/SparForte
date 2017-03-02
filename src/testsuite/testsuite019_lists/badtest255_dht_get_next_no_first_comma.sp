t : dynamic_hash_tables.table( string );
dynamic_hash_tables.set( t, "foo", "bar" );
s : string;
eof : boolean;
dynamic_hash_tables.get_first( t, s, eof );
dynamic_hash_tables.next_first( t s, eof ); -- should be comma

