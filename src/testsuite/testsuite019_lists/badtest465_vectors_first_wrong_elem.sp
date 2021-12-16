type foo is new integer;
v : vectors.vector( natural, foo );
c : vectors.cursor( natural, integer );

vectors.first( v, c ); -- c has wrong element type

