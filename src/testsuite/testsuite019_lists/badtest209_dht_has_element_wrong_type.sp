t : dynamic_hash_tables.table;
b : boolean;
dynamic_hash_tables.new_table( t, string );
b := dynamic_hash_tables.has_element( t, 1234 ); -- should be string

