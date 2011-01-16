#!/usr/local/bin/bush

pragma annotate( "shuffle" );
pragma annotate( "" );
pragma annotate( "Implement the Knuth shuffle (aka the" );
pragma annotate( "Fisher-Yates-Durstenfeld shuffle)" );
pragma annotate( "for an integer array (or, if possible, an array of any" );
pragma annotate( "type). The Knuth shuffle is used to create a random" );
pragma annotate( "permutation of an array." );
pragma annotate( "http://rosettacode.org/wiki/Knuth_shuffle" );
pragma annotate( "" );
pragma annotate( "Note: Bush has a built-in arrays.shuffle() function." );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure shuffle is

  subtype array_element_type is string;
  type stuff is array(1..3) of array_element_type;

  a : stuff := ( "bell", "book", "candle" );
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

