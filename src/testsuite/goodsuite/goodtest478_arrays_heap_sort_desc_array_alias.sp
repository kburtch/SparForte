-- testing that text_io.open and text_io.close work with file_type arrays

type atype is array(1..2) of integer;
a : atype := (2, 1);
aalias : atype renames a;

arrays.heap_sort_descending(aalias);
pragma assert( a(1) = 2 );
pragma assert( a(2) = 1 );

