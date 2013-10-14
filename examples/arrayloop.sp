#!/usr/local/bin/spar

pragma annotate( summary, "arrayloop" );
pragma annotate( description, "Loop over multiple arrays simultaneously" );
pragma annotate( description, "You are encouraged to solve this task according to the" );
pragma annotate( description, "task description, using any language you may know. Loop" );
pragma annotate( description, "over multiple arrays (or lists or tuples or whatever" );
pragma annotate( description, "they're called in your language) and print the ith" );
pragma annotate( description, "element of each. Use your language's 'for each' loop if" );
pragma annotate( description, "it has one, otherwise iterate through the collection in" );
pragma annotate( description, "order with some other loop." );
pragma annotate( description, "" );
pragma annotate( description, "For this example, loop over the arrays (a,b,c), (A,B,C)" );
pragma annotate( description, "and (1,2,3) to produce the output" );
pragma annotate( description, "" );
pragma annotate( description, "aA1" );
pragma annotate( description, "bB2" );
pragma annotate( description, "cC3" );
pragma annotate( description, "" );
pragma annotate( description, "If possible, also describe what happens when the arrays" );
pragma annotate( description, "are of different lengths. " );
pragma annotate( see_also, "http://rosettacode.org/wiki/Loop_over_multiple_arrays_simultaneously" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

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

-- VIM editor formatting instructions
-- vim: ft=spar

