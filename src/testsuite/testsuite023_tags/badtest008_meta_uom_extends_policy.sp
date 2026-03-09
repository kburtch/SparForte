-- This should fail

meta policy newpolicy is new meta; -- is missing
meta newunit is new newpolicy; -- newpolicy not a unit-of-measure

i : constant integer := 5;
pragma assert( not tags.contains_policy( i, newpolicy ) );
pragma assert( not tags.contains_unit( i, newunit ) );

