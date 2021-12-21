v1 : vectors.vector( natural, string );
v2 : vectors.vector( natural, integer );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, string );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, 123 );
vectors.first( v1, c2 );

vectors.insert_vector_and_mark( v1, c1, v2, c2 ); -- element mismatch between v2 and c2

