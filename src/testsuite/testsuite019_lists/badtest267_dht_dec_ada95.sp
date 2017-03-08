t : dynamic_hash_tables.table( integer );
dynamic_hash_tables.set( t, "foo", 1 );
pragma ada_95;
dynamic_hash_tables.decrement( t, "foo" ); -- not allowed

