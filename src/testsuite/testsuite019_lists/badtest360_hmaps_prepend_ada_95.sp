m : hashed_maps.map( string, string );

pragma ada_95;
hashed_maps.insert( m, "foo", "bar" );
hashed_maps.prepend( m, "foo", "baz" ); -- not with ada_95

