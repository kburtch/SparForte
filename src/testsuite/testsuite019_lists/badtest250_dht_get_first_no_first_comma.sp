t : dynamic_hash_tables.table;
dynamic_hash_tables.new_table( t, string );
dynamic_hash_tables.set( t, "foo", "bar" );
s : string;
eof : boolean;
dynamic_hash_tables.get_first( t s, eof ); -- should be comma

