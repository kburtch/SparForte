m : hashed_maps.map( string, string );

hashed_maps.insert( m, "foo", "bar" );
hashed_maps.decrement( m, "foo" ); -- not numeric

