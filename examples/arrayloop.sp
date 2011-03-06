#!/usr/local/bin/bush

pragma annotate( "arrayloop" );
pragma annotate( "" );
pragma annotate( "Loop over multiple arrays simultaneously" );
pragma annotate( "You are encouraged to solve this task according to the" );
pragma annotate( "task description, using any language you may know. Loop" );
pragma annotate( "over multiple arrays (or lists or tuples or whatever" );
pragma annotate( "they're called in your language) and print the ith" );
pragma annotate( "element of each. Use your language's 'for each' loop if" );
pragma annotate( "it has one, otherwise iterate through the collection in" );
pragma annotate( "order with some other loop." );
pragma annotate( "" );
pragma annotate( "For this example, loop over the arrays (a,b,c), (A,B,C)" );
pragma annotate( "and (1,2,3) to produce the output" );
pragma annotate( "" );
pragma annotate( "aA1" );
pragma annotate( "bB2" );
pragma annotate( "cC3" );
pragma annotate( "" );
pragma annotate( "If possible, also describe what happens when the arrays" );
pragma annotate( "are of different lengths. " );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Loop_over_multiple_arrays_simultaneously" );
pragma annotate( "by Ken O. Burtch" );

pragma restriction( no_external_commands );

procedure arrayloop is
  a1 : array( 1..3 ) of character := ('a', 'b', 'c');
  a2 : array( 1..3 ) of character := ('A', 'B', 'C');
  a3 : array( 1..3 ) of integer   := (1, 2, 3);
begin
  for i in arrays.first( a1 )..arrays.last( a1 ) loop
      put( a1( i ) )
        @( a2( i ) )
        @( strings.trim( strings.image( a3( i ) ), trim_end.both ) );
      new_line;
  end loop;
end arrayloop;

