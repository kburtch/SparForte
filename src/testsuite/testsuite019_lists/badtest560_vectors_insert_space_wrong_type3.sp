v  : vectors.vector( natural, string );
c1 : vectors.cursor( natural, string );
--c2 : vectors.cursor( natural, string );

vectors.insert_space( v, c1, "foo" ); -- param 3 after cursor is not cursor

