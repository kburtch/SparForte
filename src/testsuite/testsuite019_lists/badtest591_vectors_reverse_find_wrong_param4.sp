v  : vectors.vector( natural, string );
c1 : vectors.cursor( natural, string );
--c2 : vectors.cursor( natural, string );

m : integer;

vectors.reverse_find( v, "foo", c1, m ); -- param 4 is the wrong type

