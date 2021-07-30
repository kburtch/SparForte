m : hashed_maps.map( string, string );
c : hashed_maps.cursor( string, string );
k : string := "foo";

hashed_maps.first( m, c );
hashed_maps.next( k );


