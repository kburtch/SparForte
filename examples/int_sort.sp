#!/usr/local/bin/spar

pragma annotate( summary, "int_sort" );
pragma annotate( description, "Sort an array (or list) of integers in ascending" );
pragma annotate( description, "numerical order. Use a sorting facility provided by" );
pragma annotate( description, "the language/library if possible." );
pragma annotate( see_also, "http://rosettacode.org/wiki/Sort_an_integer_array" );
pragma annotate( author, "Ken O. Burtch" );
pragma license( unrestricted );

pragma restriction( no_external_commands );

procedure int_sort is
  type int_array is array (1..9) of integer;
  int_values : int_array := (0,1,8,2,7,3,6,4,5);
begin
  arrays.heap_sort( int_values );
  for i in arrays.first( int_values )..arrays.last( int_values ) loop
    ? int_values(i);
  end loop;
end int_sort;

-- VIM editor formatting instructions
-- vim: ft=spar

