-- This should fail

meta newpolicy new meta; -- is missing

i : constant integer := 5;
pragma assert( not tags.contains_unit( i, newpolicy ) );

