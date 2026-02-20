-- testing that text_io.open and text_io.close work with file_type arrays

meta fakeunit is new meta;

type atype is array(1..2) of integer;

a : atype := (1 tagged fakeunit, 2 tagged fakeunit);
i : integer;

i := arrays.length(a);
pragma assert( not tags.has_unit( i ) );

