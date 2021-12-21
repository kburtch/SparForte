v1 : vectors.vector( natural, string );
v2 : vectors.vector( positive, string );
c  : vectors.cursor( natural, string );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, "bar" );
vectors.first( v1, c );

vectors.insert_vector( v1, c, v2 ); -- index mismatch between v1 and c

