v : vectors.vector( natural, string );
c : vectors.cursor( natural, string );

m : integer;

vectors.append_elements( v, "foo", 2 );
vectors.first( v, c );

vectors.next; -- no param

