m : hashed_maps.map( string, string );

pragma ada_95;
hashed_maps.insert( m, "foo", "bar" );
? hashed_maps.extract( m, "foo" ); -- not with ada_95

