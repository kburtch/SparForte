type a is array(0..1) of integer;
t : dynamic_hash_tables.table( a ); -- array not allowed
pragma assumption( used, t );
