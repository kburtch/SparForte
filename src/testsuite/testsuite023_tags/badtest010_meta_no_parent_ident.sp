-- This should fail

meta newunit is new ; -- no parent

i : constant integer := 5;
pragma assert( not tags.contains_unit( i, newunit ) );

