-- testing image that is returned

meta policy fakepolicy1 is new meta;

i : integer := 5 tagged policy fakepolicy1;
s : string := tags.get_policies_image( i );

pragma assert( s = "fakepolicy1" );

