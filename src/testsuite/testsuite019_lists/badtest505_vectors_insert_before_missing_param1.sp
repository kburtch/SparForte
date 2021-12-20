v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

vectors.append_elements( v, "foo" );
vectors.first( v, c );
vectors.insert_before( c, "foo", "bar" ); -- param 1 missing

