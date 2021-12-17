type foo is new integer;

v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

m : constant foo := 1;

vectors.append_elements( v, "foo", 2 );
vectors.last( v, c );

vectors.delete( v, 0, m ); -- m is not vector

