m : hashed_maps.map( string, integer );

pragma ada_95;
hashed_maps.insert( m, "foo", 1 );
hashed_maps.increment( m, "foo" ); -- not with ada_95

