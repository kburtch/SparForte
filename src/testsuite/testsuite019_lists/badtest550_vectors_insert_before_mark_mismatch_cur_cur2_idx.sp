v1 : vectors.vector( positive, string );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( positive, string );

vectors.append_elements( v1, "foo" );
vectors.first( v1, c1 );

-- this test might be redundant

vectors.insert_before_and_mark( v1, c1, "foo", c2 ); -- c1, c2 mismatch

