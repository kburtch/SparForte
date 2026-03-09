-- testing image that is returned

meta fakeunit1 is new meta;

i : integer := 5 tagged fakeunit1;
s : string := tags.get_unit_image( i );

pragma assert( s = "fakeunit1" );

