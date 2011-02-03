#!/usr/local/bin/bush

pragma annotate( "int_array" );
pragma annotate( "" );
pragma annotate( "Sort an array (or list) of integers in ascending" );
pragma annotate( "numerical order. Use a sorting facility provided by" );
pragma annotate( "the language/library if possible." );
pragma annotate( "" );
pragma annotate( "http://rosettacode.org/wiki/Sort_an_integer_array" );
pragma annotate( "by Ken O. Burtch (based on Ada version)" );

pragma restriction( no_external_commands );

procedure int_sort is
  type int_array is array (1..9) of integer;
  values : int_array := (0,1,8,2,7,3,6,4,5);
begin
  arrays.heap_sort( values );
  for i in arrays.first( values )..arrays.last( values ) loop
    ? values(i);
  end loop;
end int_sort;

