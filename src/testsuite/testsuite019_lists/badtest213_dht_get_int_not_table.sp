t : dynamic_hash_tables.table( string );
i : integer := 1;
s : string;
dynamic_hash_tables.set( t, "foo", "bar" );
s := dynamic_hash_tables.get( i, "foo" ); -- should be t

