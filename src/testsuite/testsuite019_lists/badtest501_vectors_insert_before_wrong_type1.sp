v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v, "foo" );
vectors.first( v, c );
vectors.insert_before(m, c, "foo" ); -- param 1 wrong type

