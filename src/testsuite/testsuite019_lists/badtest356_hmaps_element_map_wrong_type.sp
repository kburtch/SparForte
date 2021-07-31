m : hashed_maps.map( string, string );
m2 : string := "foo";

hashed_maps.insert( m, "foo", "bar" );
?  hashed_maps.element( m2, "foo" ); -- map wrong type

