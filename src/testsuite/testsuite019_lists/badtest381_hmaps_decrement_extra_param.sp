m : hashed_maps.map( string, integer );

hashed_maps.insert( m, "foo", 0 );
hashed_maps.decrement( m, "bar",1 ,1 ); -- too many params

