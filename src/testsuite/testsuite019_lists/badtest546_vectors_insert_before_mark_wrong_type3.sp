v1 : vectors.vector( natural, string );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v1, "foo" );
vectors.first( v1, c1 );

vectors.insert_before_and_mark( v1, c1, m, c2 ); -- param 3 wrong type

