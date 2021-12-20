v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v, "foo" );
vectors.first( v, c );
vectors.insert_before( v, c, 15 ); -- param 3 wrong type

