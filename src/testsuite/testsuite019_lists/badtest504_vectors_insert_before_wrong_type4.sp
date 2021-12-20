v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v, "foo" );
vectors.first( v, c );
vectors.insert_before( v, c, "foo", "bar" ); -- optional param 4 wrong type

