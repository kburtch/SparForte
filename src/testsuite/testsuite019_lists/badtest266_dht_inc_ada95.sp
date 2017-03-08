t : dynamic_hash_tables.table( integer );
dynamic_hash_tables.set( t, "foo", 1 );
pragma ada_95;
dynamic_hash_tables.increment( t, "foo" ); -- not allowed

