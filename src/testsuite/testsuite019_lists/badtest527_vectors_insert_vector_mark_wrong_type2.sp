v1 : vectors.vector( natural, string );
v2 : vectors.vector( natural, string );
--c1  : vectors.cursor( natural, string );
c2  : vectors.cursor( natural, string );

m : constant integer := 1;

vectors.insert_vector( v1, m, v2, c2 ); -- m is not a cursor

