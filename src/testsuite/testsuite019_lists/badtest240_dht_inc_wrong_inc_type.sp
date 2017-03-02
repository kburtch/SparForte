t : dynamic_hash_tables.table( integer );
dynamic_hash_tables.set( t, "counter", 0 );
dynamic_hash_tables.increment( t, "counter", "foo" ); -- should be int

