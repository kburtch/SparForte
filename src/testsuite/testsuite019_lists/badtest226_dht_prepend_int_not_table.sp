t : dynamic_hash_tables.table( string );
i : integer := 1;
dynamic_hash_tables.set( t, "foo", "bar" );
dynamic_hash_tables.prepend( i, "foo", "bar" ); -- should be t

