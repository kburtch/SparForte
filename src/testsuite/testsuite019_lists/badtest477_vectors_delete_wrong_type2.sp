v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v, "foo", 2 );
vectors.last( v, c );

vectors.delete( m, 0 ); -- m is not vector

