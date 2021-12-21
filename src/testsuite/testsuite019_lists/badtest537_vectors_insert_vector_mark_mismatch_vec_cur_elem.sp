v1 : vectors.vector( natural, integer );
v2 : vectors.vector( natural, string );
c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, string );

vectors.append_elements( v1, 123 );
vectors.append_elements( v2, "bar" );
--vectors.first( v1, c1 );

vectors.insert_vector_and_mark( v1, c1, v2, c2 ); -- element mismatch between v1 and c

