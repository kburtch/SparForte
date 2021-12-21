v1 : vectors.vector( positive, string );
v2 : vectors.vector( natural, string );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, string );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, "bar" );
--vectors.first( v1, c );

vectors.insert_vector_and_mark( v1, c1, v2, c2 ); -- index mismatch between v1 and c1

