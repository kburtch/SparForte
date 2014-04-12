t : dynamic_hash_tables.table;
i : integer := 1;
dynamic_hash_tables.new_table( t, string );
dynamic_hash_tables.set( i, "foo", "bar" ); -- should be t

