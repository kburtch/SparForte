v1 : vectors.vector( integer, string );
v2 : vectors.vector( positive, string );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, "bar" );
vectors.move( v1, v2 ); -- incompatible indices

