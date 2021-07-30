--m : hashed_maps.map( string, string );
c : hashed_maps.cursor( string, string );
m : string := "foo";

hashed_maps.first( m, c ); -- map wrong type

