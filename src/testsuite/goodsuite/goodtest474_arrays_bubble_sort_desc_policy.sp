-- testing that text_io.open and text_io.close work with file_type arrays

meta policy fakepolicy1 is new meta;
meta policy fakepolicy2 is new meta;

type atype is array(1..2) of integer;
a : atype := (1 tagged policy fakepolicy1, 2 tagged policy fakepolicy2);

arrays.bubble_sort_descending(a);
pragma assert( tags.contains_policy( a(1), fakepolicy2) );
pragma assert( tags.contains_policy( a(2), fakepolicy1) );

