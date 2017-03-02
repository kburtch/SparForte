t : dynamic_hash_tables.table( integer );
dynamic_hash_tables.set( t, "counter", 0 );
i : integer := 1;
dynamic_hash_tables.decrement( i, "counter" ); -- should be t

