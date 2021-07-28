m : hashed_maps.map( string, string );
k : integer := 1;

hashed_maps.insert( m, "foo", "bar" );
hashed_maps.replace( m, "foo", k ); -- map wrong type

