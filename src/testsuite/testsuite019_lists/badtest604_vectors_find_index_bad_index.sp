v  : vectors.vector( natural, string );
i1   : constant natural := 2;
i2   : natural;
pragma assumption( factor, i2 );

vectors.find_index( v, "foo", i1, i2 ); -- index is out-of-range

