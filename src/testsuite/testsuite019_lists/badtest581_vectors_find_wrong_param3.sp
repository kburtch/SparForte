v  : vectors.vector( natural, string );
--c1 : vectors.cursor( natural, string );
c2 : vectors.cursor( natural, string );

m : integer;

vectors.find( v, "foo", m, c2 ); -- param 3 is the wrong type

