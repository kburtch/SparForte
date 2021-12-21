v1 : vectors.vector( natural, integer );
v2 : vectors.vector( natural, string );
c  : vectors.cursor( natural, string );

vectors.append_elements( v1, 123 );
vectors.append_elements( v2, "bar" );
--vectors.first( v1, c );

vectors.insert_vector( v1, c, v2 ); -- element mismatch between v1 and c

