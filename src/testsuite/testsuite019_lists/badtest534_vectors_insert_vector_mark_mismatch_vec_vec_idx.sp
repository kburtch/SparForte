v1 : vectors.vector( natural, string );
v2 : vectors.vector( positive, string );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, string );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, "bar" );
vectors.first( v1, c1);

vectors.insert_vector_and_mark( v1, c2, v2, c2 ); -- index mismatch between v2 and c1

