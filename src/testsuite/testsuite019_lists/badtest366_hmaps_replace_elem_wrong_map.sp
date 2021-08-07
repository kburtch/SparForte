m1 : hashed_maps.map( string, string );
m2 : hashed_maps.map( string, integer );
c : hashed_maps.cursor( string, string );

hashed_maps.insert( m1, "foo", "bar" );
hashed_maps.first( m1, c );
hashed_maps.replace_element( m2, c, "foo" ); -- wrong map

