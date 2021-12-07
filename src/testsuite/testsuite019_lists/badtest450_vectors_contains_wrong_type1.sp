v : vectors.vector( positive, string );
m : integer;

vectors.append_elements( v, "foo" );
? vectors.contains( m, "foo" ); -- m is not vector

