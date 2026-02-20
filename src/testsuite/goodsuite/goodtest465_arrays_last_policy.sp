-- testing that text_io.open and text_io.close work with file_type arrays

meta policy fakepolicy is new meta;

type atype is array(1..2) of integer;

a : atype := (1 tagged policy fakepolicy, 2 tagged policy fakepolicy);
i : integer;

i := arrays.last(a);
pragma assert( not tags.has_unit( i ) );

