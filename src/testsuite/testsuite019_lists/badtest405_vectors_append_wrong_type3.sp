v : vectors.vector( natural, string );
i : constant natural := 0;

vectors.append_elements( v, "foo" );
vectors.append( v, i, 15  ); -- must be string

