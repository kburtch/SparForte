#!/usr/local/bin/spar

pragma annotate( summary, "arrayloop" )
       @( description, "Loop over multiple arrays simultaneously" )
       @( description, "You are encouraged to solve this task according to the" )
       @( description, "task description, using any language you may know. Loop" )
       @( description, "over multiple arrays (or lists or tuples or whatever" )
       @( description, "they're called in your language) and print the ith" )
       @( description, "element of each. Use your language's 'for each' loop if" )
       @( description, "it has one, otherwise iterate through the collection in" )
       @( description, "order with some other loop." )
       @( description, "" )
       @( description, "For this example, loop over the arrays (a,b,c), (A,B,C)" )
       @( description, "and (1,2,3) to produce the output" )
       @( description, "" )
       @( description, "aA1" )
       @( description, "bB2" )
       @( description, "cC3" )
       @( description, "" )
       @( description, "If possible, also describe what happens when the arrays" )
       @( description, "are of different lengths. " )
       @( category, "tutorials" )
       @( author, "Ken O. Burtch" )
       @( see_also, "http://rosettacode.org/wiki/Loop_over_multiple_arrays_simultaneously" );
pragma license( unrestricted );

pragma software_model( nonstandard );
pragma restriction( no_external_commands );

procedure arrayloop is
  a1 : constant array( 1..3 ) of character := ('a', 'b', 'c');
  a2 : constant array( 1..3 ) of character := ('A', 'B', 'C');
  a3 : constant array( 1..3 ) of integer   := (1, 2, 3);
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

