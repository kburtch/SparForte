#!/usr/local/bin/spar

pragma annotate( summary, "shuffle" );
pragma annotate( description, "Implement the Knuth shuffle (aka the" );
pragma annotate( description, "Fisher-Yates-Durstenfeld shuffle)" );
pragma annotate( description, "for an integer array (or, if possible, an array of any" );
pragma annotate( description, "type). The Knuth shuffle is used to create a random" );
pragma annotate( description, "permutation of an array." );
pragma annotate( description, "Note: spar has a built-in arrays.shuffle() function that does this." );
pragma annotate( category, "algorithms" );
pragma annotate( see_also, "http://rosettacode.org/wiki/Knuth_shuffle" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure shuffle is

  subtype array_element_type is string;
  type magic_items is array(1..3) of array_element_type;

  a : magic_items := ( "bell", "book", "candle" );
  t : array_element_type;
  k : integer;

begin

  for i in reverse arrays.first( a ) .. arrays.last( a )-1 loop
    k := integer( numerics.rnd( i+1 ) ) - 1 + arrays.first(a);
    t := a(i);
    a(i) := a(k);
    a(k) := t;
  end loop;

  for i in arrays.first( a ) .. arrays.last( a ) loop
    ? a(i);
  end loop;

end shuffle;

-- VIM editor formatting instructions
-- vim: ft=spar

