t : dynamic_hash_tables.table;
i : integer := 1;
s : string;
dynamic_hash_tables.new_table( t, string );
dynamic_hash_tables.set( t, "foo", "bar" );
s := dynamic_hash_tables.get( i, "foo" ); -- should be t

