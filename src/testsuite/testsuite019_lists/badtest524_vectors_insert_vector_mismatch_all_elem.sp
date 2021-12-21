v1 : vectors.vector( natural, string );
v2 : vectors.vector( natural, integer );
c  : vectors.cursor( natural, boolean );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, 123 );
--vectors.first( v1, c );

vectors.insert_vector( v1, c, v2 ); -- all element mismatch

