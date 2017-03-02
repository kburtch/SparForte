t : dynamic_hash_tables.table( string );
dynamic_hash_tables.set( t, "foo", "bar" );
dynamic_hash_tables.append( t, "foo", 1234 ); -- should be string

