t : dynamic_hash_tables.table;
i : integer := 1;
b : boolean;
dynamic_hash_tables.new_table( t, string );
b := dynamic_hash_tables.has_element( i, "foo" ); -- should be table

