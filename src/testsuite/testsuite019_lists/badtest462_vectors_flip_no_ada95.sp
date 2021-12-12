pragma ada_95;
v : vectors.vector( natural, string );
vectors.append_elements( v, "foo" );

vectors.flip( v ); -- not allowed with ada_95

