v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

vectors.append_elements( v, "foo" );
vectors.first( v, c );
vectors.insert_before( v, c, "foo", -1 ); -- negative count

