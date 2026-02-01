-- testing that text_io.open and text_io.close work with file_type arrays

meta fakeunit1 is new meta;
meta fakeunit2 is new meta;

type atype is array(1..2) of integer;
a : atype := (1 tagged fakeunit1, 2 tagged fakeunit2);

arrays.heap_sort_descending(a);
pragma assert( tags.contains_unit( a(1), fakeunit2) );
pragma assert( tags.contains_unit( a(2), fakeunit1) );

