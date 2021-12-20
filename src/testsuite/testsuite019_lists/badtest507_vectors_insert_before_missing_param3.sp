v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

-- certain combinations are hard to test since there's an optional param 4

vectors.append_elements( v, "foo" );
vectors.first( v, c );
vectors.insert_before( v, c ); -- param 2 missing

