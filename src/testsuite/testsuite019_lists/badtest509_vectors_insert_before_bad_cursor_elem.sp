v1 : vectors.vector( natural, string );
v2 : vectors.vector( natural, natural );
c : vectors.cursor( natural, string );

vectors.append_elements( v1, "foo" );
vectors.first( v2, c );
vectors.insert_before( v1, c, "foo" ); -- cursor for wrong vector

