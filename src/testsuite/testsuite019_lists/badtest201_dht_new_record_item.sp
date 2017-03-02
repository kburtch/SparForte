type r is record
     foo : integer;
end record;
t : dynamic_hash_tables.table( r ); -- record is not allowed
pragma assumption( used, t );
