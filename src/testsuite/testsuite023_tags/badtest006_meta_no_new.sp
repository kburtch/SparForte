-- This should fail

meta newpolicy is meta; -- new is missing

i : constant integer := 5;
pragma assert( not tags.contains_unit( i, newpolicy ) );

