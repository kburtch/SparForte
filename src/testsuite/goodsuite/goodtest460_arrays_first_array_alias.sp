-- testing that text_io.open and text_io.close work with file_type arrays

type atype is array(1..2) of integer;
a : atype := (1, 2);
aalias : atype renames a;
i : integer;

i := arrays.first(aalias);
pragma assert( i = 1 );

