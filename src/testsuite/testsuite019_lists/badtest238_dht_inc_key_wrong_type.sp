t : dynamic_hash_tables.table( integer );
dynamic_hash_tables.set( t, "counter", 0 );
dynamic_hash_tables.increment( t, 1234 ); -- should be string

