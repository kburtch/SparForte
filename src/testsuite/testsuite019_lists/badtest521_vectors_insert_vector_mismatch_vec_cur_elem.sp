v1 : vectors.vector( natural, string );
v2 : vectors.vector( natural, string );
c  : vectors.cursor( natural, integer );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, "bar" );

vectors.insert_vector( v1, c, v2 ); -- element mismatch between v1 and c

