m : hashed_maps.map( string, string );
c : hashed_maps.cursor( string, string );
k : constant string := "foo";
b : string := "false";

hashed_maps.insert( m, k, k, c, b );

