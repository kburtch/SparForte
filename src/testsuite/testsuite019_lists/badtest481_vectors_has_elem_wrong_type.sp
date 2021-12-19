v : vectors.vector( natural, string );
--c : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v, "foo", 2 );

? vectors.has_element( m ); -- m is not vector

