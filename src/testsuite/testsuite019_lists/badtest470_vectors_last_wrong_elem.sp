type foo is new integer;
v : vectors.vector( natural, foo );
c : vectors.cursor( natural, integer );

vectors.last( v, c ); -- c has wrong element type

