type r is record
     foo : integer;
end record;
t : dynamic_hash_tables.table;
dynamic_hash_tables.new_table( t, r ); -- record not allowed

