v1 : vectors.vector( natural, string );
c1 : vectors.cursor( natural, integer );
c2 : vectors.cursor( natural, string );

vectors.append_elements( v1, "foo" );
--vectors.first( v1, c1 );

vectors.insert_before_and_mark( v1, c1, "foo", c2 ); -- v1, c1 mismatch
