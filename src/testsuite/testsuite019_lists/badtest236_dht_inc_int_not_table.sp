t : dynamic_hash_tables.table;
dynamic_hash_tables.new_table( t, integer );
dynamic_hash_tables.set( t, "counter", 0 );
i : integer := 1;
dynamic_hash_tables.increment( i, "counter" ); -- should be t

