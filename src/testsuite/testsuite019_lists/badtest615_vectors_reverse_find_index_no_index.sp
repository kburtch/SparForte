v  : vectors.vector( natural, string );
i1   : natural;
i2   : natural;
pragma assumption( factor, i2 );

vectors.reverse_find_index( v, "foo", i1, i2 ); -- index is out-of-range

