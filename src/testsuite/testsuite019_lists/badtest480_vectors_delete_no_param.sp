v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

vectors.append_elements( v, "foo", 2 );
vectors.last( v, c );

vectors.delete; -- no parameters

