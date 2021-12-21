v1 : vectors.vector( natural, integer );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, integer );

-- this test might be redundant

vectors.insert_before_and_mark( v1, c1, 123, c2 ); -- c1, c2 mismatch

