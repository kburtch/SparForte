t : dynamic_hash_tables.table;
s : string;
dynamic_hash_tables.new_table( t, string );
dynamic_hash_tables.set( t, "foo", "bar" );
s := dynamic_hash_tables.get( t, 1234 ); -- should be string

