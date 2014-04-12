t : dynamic_hash_tables.table;
dynamic_hash_tables.new_table( t, string );
dynamic_hash_tables.set( t, "foo", "bar" );
dynamic_hash_tables.replace( t, "foo", 1234 ); -- should be string

