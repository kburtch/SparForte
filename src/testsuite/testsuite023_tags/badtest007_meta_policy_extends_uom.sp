-- This should fail

meta newunit is new meta; -- is missing
meta policy newpolicy is new newunit; -- newunit not a policy

i : constant integer := 5;
pragma assert( not tags.contains_policy( i, newpolicy ) );
pragma assert( not tags.contains_unit( i, newunit ) );

