v1 : vectors.vector( natural, string );
v2 : vectors.vector( natural, integer );

vectors.append_elements( v1, "foo" );
vectors.append_elements( v2, 15 );
vectors.move( v1, v2 ); -- incompatible keys

