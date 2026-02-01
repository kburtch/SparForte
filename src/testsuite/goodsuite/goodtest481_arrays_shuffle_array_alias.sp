-- testing that text_io.open and text_io.close work with file_type arrays

type atype is array(1..2) of integer;
a : atype := (2, 1);
aalias : atype renames a;

arrays.shuffle(aalias);
pragma assert( a(1) = 1 or a(1) = 2 );
pragma assert( a(2) = 1 or a(2) = 2 );

