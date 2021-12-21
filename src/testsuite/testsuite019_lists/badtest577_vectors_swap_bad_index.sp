v  : vectors.vector( natural, string );
--c1 : vectors.cursor( natural, string );
--c2 : vectors.cursor( natural, string );

vectors.append_elements( v, "foo" );
vectors.append_elements( v, "bar" );

vectors.swap( v, 2, 3 ); -- index positions 2 and 3 are too high

