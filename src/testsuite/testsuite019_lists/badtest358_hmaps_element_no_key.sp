m : hashed_maps.map( string, string );
k : string := "baz";

hashed_maps.insert( m, "foo", "bar" );
?  hashed_maps.element( m, k ); -- key does not exist

